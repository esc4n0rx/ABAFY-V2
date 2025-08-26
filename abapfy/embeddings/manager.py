import numpy as np
import os
import requests
from typing import List, Dict, Tuple, Optional
import pickle
from pathlib import Path
import hashlib
import time
from dotenv import load_dotenv

# Import oficial do Hugging Face
try:
    from huggingface_hub import InferenceClient
    HF_HUB_AVAILABLE = True
except ImportError:
    HF_HUB_AVAILABLE = False
    InferenceClient = None

load_dotenv()

class EmbeddingManager:
    """Gerenciador de embeddings para código ABAP usando Hugging Face"""
    
    def __init__(self, model_name: str = "sentence-transformers/all-MiniLM-L6-v2", verbose: bool = False):
        # Modelos que funcionam com feature-extraction
        self.available_models = [
            "sentence-transformers/all-MiniLM-L6-v2",
            "sentence-transformers/paraphrase-MiniLM-L6-v2",
            "sentence-transformers/multi-qa-MiniLM-L6-cos-v1",
            "sentence-transformers/all-mpnet-base-v2",
            "BAAI/bge-small-en-v1.5",
            "jinaai/jina-embeddings-v2-small-en"
        ]
        
        self.model_name = model_name
        self.current_model = model_name
        self.verbose = verbose  # Controla verbosidade dos logs
        
        # Obter token da API do Hugging Face
        self.api_token = os.getenv('HF_TOKEN') or os.getenv('HF_API_TOKEN') or os.getenv('HUGGINGFACE_API_TOKEN')
        
        if not self.api_token:
            if self.verbose:
                print("❌ Token do Hugging Face não configurado!")
                print("💡 Configure HF_TOKEN, HF_API_TOKEN ou HUGGINGFACE_API_TOKEN")
            self.offline_mode = True
            self.client = None
        else:
            if self.verbose:
                print(f"🔑 Token HF encontrado: {self.api_token[:8]}...{self.api_token[-4:]}")
            self.offline_mode = False
            self._init_client()
        
        self.cache_dir = Path.home() / ".abapfy" / "embeddings_cache"
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        
        # Headers para requests diretos
        self.headers = {
            "Authorization": f"Bearer {self.api_token}",
            "Content-Type": "application/json",
            "User-Agent": "ABAPFY/1.0.0"
        }
    
    def _init_client(self):
        """Inicializa o cliente do Hugging Face"""
        self.client = None
        self.use_requests = False
        
        if HF_HUB_AVAILABLE:
            try:
                # Usar provider hf-inference (padrão oficial)
                self.client = InferenceClient(
                    provider="hf-inference",
                    api_key=self.api_token
                )
                if self.verbose:
                    print("✅ Cliente Hugging Face inicializado com provider hf-inference")
                
                # Testar conexão
                if not self._test_client():
                    if self.verbose:
                        print("⚠️ Cliente HF falhou, usando requests diretos...")
                    self.client = None
                    self.use_requests = True
                    
            except Exception as e:
                if self.verbose:
                    print(f"⚠️ Erro ao inicializar cliente: {str(e)}")
                try:
                    self.client = InferenceClient(api_key=self.api_token)
                    if self.verbose:
                        print("✅ Cliente Hugging Face inicializado (sem provider)")
                    if not self._test_client():
                        self.use_requests = True
                except Exception:
                    self.use_requests = True
        else:
            self.use_requests = True
        
        # Se usar requests, testar um modelo
        if self.use_requests and self.verbose:
            print("💡 Usando requests diretos para API do Hugging Face...")
            self._test_requests_api()
    
    def _test_client(self) -> bool:
        """Testa o cliente HF Hub com validação corrigida"""
        if not self.client or self.offline_mode:
            return False
        
        if self.verbose:
            print("🔍 Testando cliente Hugging Face...")
        
        for model in self.available_models[:2]:  # Testar apenas 2 modelos
            try:
                if self.verbose:
                    print(f"🧪 Testando modelo: {model}")
                
                # Teste simples
                result = self.client.feature_extraction(
                    "Hello world test",
                    model=model
                )
                
                # Validação corrigida - verificar se existe e não está vazio
                if result is not None:
                    # Converter para numpy se necessário
                    if hasattr(result, '__len__'):  # Se tem length
                        if len(result) > 0:  # Se não está vazio
                            if self.verbose:
                                print(f"✅ Modelo HF funcional: {model}")
                            self.current_model = model
                            return True
                
            except Exception as e:
                error_msg = str(e)
                if "truth value of an array" in error_msg:
                    if self.verbose:
                        print(f"✅ Modelo funcional (array result): {model}")
                    self.current_model = model
                    return True
                elif self.verbose and "doesn't support task" not in error_msg:
                    print(f"⚠️ Erro em {model}: {error_msg[:60]}...")
                continue
        
        return False
    
    def _test_requests_api(self):
        """Testa API usando requests diretos"""
        if self.verbose:
            print("🔍 Testando API com requests diretos...")
        
        for model in self.available_models[:2]:
            try:
                if self.verbose:
                    print(f"🧪 Testando modelo via requests: {model}")
                
                url = f"https://api-inference.huggingface.co/models/{model}"
                payload = {"inputs": "Hello world test"}
                
                response = requests.post(
                    url,
                    headers=self.headers,
                    json=payload,
                    timeout=15
                )
                
                if response.status_code == 200:
                    result = response.json()
                    if result and len(result) > 0:
                        if self.verbose:
                            print(f"✅ Modelo requests funcional: {model}")
                        self.current_model = model
                        return
                elif response.status_code == 503:
                    if self.verbose:
                        print(f"⏳ Modelo {model} carregando...")
                    self.current_model = model
                    return
                    
            except Exception as e:
                if self.verbose:
                    print(f"⚠️ Erro requests {model}: {str(e)[:50]}...")
                continue
        
        if self.verbose:
            print("❌ Nenhum modelo funcional via requests")
        self.offline_mode = True
    
    def create_embeddings(self, chunks: List[str], progress_callback=None) -> np.ndarray:
        """Cria embeddings para lista de chunks com callback de progresso"""
        if self.offline_mode:
            if self.verbose:
                print("🔄 Modo offline - usando embeddings simulados")
            return self._create_dummy_embeddings(len(chunks))
        
        try:
            embeddings = []
            batch_size = 3
            total_batches = (len(chunks) + batch_size - 1) // batch_size
            
            # Log inicial apenas se verbose
            if self.verbose:
                print(f"🚀 Usando modelo: {self.current_model}")
                print(f"🚀 Método: {'HF Hub' if self.client and not self.use_requests else 'Requests'}")
                print(f"🚀 Processando {len(chunks)} chunks em {total_batches} batches...")
            
            success_count = 0
            fail_count = 0
            
            for i in range(0, len(chunks), batch_size):
                batch_chunks = chunks[i:i + batch_size]
                batch_num = i // batch_size + 1
                
                # Callback de progresso
                if progress_callback:
                    progress_callback(batch_num, total_batches, f"Processando batch {batch_num}")
                
                batch_embeddings = self._encode_batch(batch_chunks)
                if batch_embeddings and len(batch_embeddings) > 0:
                    embeddings.extend(batch_embeddings)
                    success_count += len(batch_chunks)
                else:
                    dummy_embeddings = [self._create_dummy_embedding() for _ in batch_chunks]
                    embeddings.extend(dummy_embeddings)
                    fail_count += len(batch_chunks)
                
                # Pausa entre batches
                if batch_num < total_batches:
                    time.sleep(1)  # Reduzido de 2 para 1 segundo
            
            if self.verbose:
                print(f"📊 Resultado: {success_count} reais, {fail_count} simulados")
            
            return np.array(embeddings)
            
        except Exception as e:
            if self.verbose:
                print(f"⚠️ Erro geral: {str(e)}")
            return self._create_dummy_embeddings(len(chunks))
    
    def _encode_batch(self, texts: List[str]) -> Optional[List[np.ndarray]]:
        """Codifica batch usando método apropriado"""
        if self.offline_mode:
            return None
        
        # Preparar textos
        processed_texts = [text[:800] for text in texts]
        
        if self.client and not self.use_requests:
            return self._encode_with_hub_client(processed_texts)
        else:
            return self._encode_with_requests(processed_texts)
    
    def _encode_with_hub_client(self, texts: List[str]) -> Optional[List[np.ndarray]]:
        """Codifica usando HF Hub client com tratamento robusto"""
        try:
            embeddings = []
            
            for i, text in enumerate(texts):
                try:
                    result = self.client.feature_extraction(
                        text,
                        model=self.current_model
                    )
                    
                    # Tratamento robusto do resultado
                    embedding = self._process_result(result)
                    if embedding is not None:
                        embeddings.append(embedding)
                    else:
                        embeddings.append(self._create_dummy_embedding())
                        
                except Exception as e:
                    error_msg = str(e)
                    if "truth value of an array" in error_msg:
                        embeddings.append(self._create_dummy_embedding())
                    else:
                        if self.verbose:
                            print(f"⚠️ Erro HF texto {i+1}: {error_msg[:40]}...")
                        embeddings.append(self._create_dummy_embedding())
                
                # Pausa entre textos reduzida
                if i < len(texts) - 1:
                    time.sleep(0.3)  # Reduzido de 0.5 para 0.3
            
            return embeddings if embeddings else None
            
        except Exception as e:
            if self.verbose:
                print(f"⚠️ Erro batch HF: {str(e)}")
            return None
    
    def _encode_with_requests(self, texts: List[str]) -> Optional[List[np.ndarray]]:
        """Codifica usando requests diretos"""
        try:
            embeddings = []
            url = f"https://api-inference.huggingface.co/models/{self.current_model}"
            
            for i, text in enumerate(texts):
                try:
                    payload = {"inputs": text}
                    
                    response = requests.post(
                        url,
                        headers=self.headers,
                        json=payload,
                        timeout=15  # Reduzido de 20 para 15
                    )
                    
                    if response.status_code == 200:
                        result = response.json()
                        embedding = self._process_result(result)
                        if embedding is not None:
                            embeddings.append(embedding)
                        else:
                            embeddings.append(self._create_dummy_embedding())
                    elif response.status_code == 503:
                        if self.verbose:
                            print(f"⏳ Modelo carregando, aguardando...")
                        time.sleep(3)  # Reduzido de 5 para 3
                        # Tentar novamente uma vez
                        response2 = requests.post(url, headers=self.headers, json=payload, timeout=20)
                        if response2.status_code == 200:
                            result = response2.json()
                            embedding = self._process_result(result)
                            if embedding is not None:
                                embeddings.append(embedding)
                            else:
                                embeddings.append(self._create_dummy_embedding())
                        else:
                            embeddings.append(self._create_dummy_embedding())
                    else:
                        if self.verbose:
                            print(f"⚠️ HTTP {response.status_code} para texto {i+1}")
                        embeddings.append(self._create_dummy_embedding())
                        
                except Exception as e:
                    if self.verbose:
                        print(f"⚠️ Erro requests texto {i+1}: {str(e)[:40]}...")
                    embeddings.append(self._create_dummy_embedding())
                
                # Pausa entre requests reduzida
                if i < len(texts) - 1:
                    time.sleep(0.5)  # Reduzido de 1 para 0.5
            
            return embeddings if embeddings else None
            
        except Exception as e:
            if self.verbose:
                print(f"⚠️ Erro batch requests: {str(e)}")
            return None
    
    def _process_result(self, result) -> Optional[np.ndarray]:
        """Processa resultado de forma robusta"""
        try:
            if result is None:
                return None
            
            # Se é numpy array
            if hasattr(result, 'shape'):
                if len(result.shape) == 1:
                    return result.astype(np.float32)
                elif len(result.shape) == 2:
                    return result[0].astype(np.float32)  # Pegar primeira linha
                else:
                    return None
            
            # Se é lista
            elif isinstance(result, list):
                if len(result) > 0:
                    # Lista de números
                    if isinstance(result[0], (int, float)):
                        return np.array(result, dtype=np.float32)
                    # Lista de listas
                    elif isinstance(result[0], list):
                        return np.array(result[0], dtype=np.float32)
                    else:
                        return None
                else:
                    return None
            
            # Outros tipos
            else:
                return None
                
        except Exception as e:
            if self.verbose:
                print(f"⚠️ Erro processando resultado: {str(e)[:30]}...")
            return None
    
    def _create_dummy_embeddings(self, count: int) -> np.ndarray:
        """Cria embeddings dummy consistentes"""
        np.random.seed(42)
        return np.random.rand(count, 384).astype(np.float32)
    
    def _create_dummy_embedding(self) -> np.ndarray:
        """Cria um embedding dummy individual"""
        np.random.seed(int(time.time() * 1000) % 1000)
        return np.random.rand(384).astype(np.float32)
    
    def get_cached_embeddings(self, content_hash: str) -> Optional[np.ndarray]:
        """Obtém embeddings do cache"""
        cache_file = self.cache_dir / f"{content_hash}.pkl"
        if cache_file.exists():
            try:
                with open(cache_file, 'rb') as f:
                    embeddings = pickle.load(f)
                    if self.verbose:
                        print(f"💾 Embeddings carregados do cache")
                    return embeddings
            except Exception as e:
                if self.verbose:
                    print(f"⚠️ Erro ao carregar cache: {e}")
                return None
        return None
    
    def cache_embeddings(self, content_hash: str, embeddings: np.ndarray):
        """Salva embeddings no cache"""
        cache_file = self.cache_dir / f"{content_hash}.pkl"
        try:
            with open(cache_file, 'wb') as f:
                pickle.dump(embeddings, f)
            if self.verbose:
                print(f"💾 Embeddings salvos no cache")
        except Exception as e:
            if self.verbose:
                print(f"⚠️ Erro ao salvar cache: {e}")
    
    def calculate_content_hash(self, content: str) -> str:
        """Calcula hash do conteúdo para cache"""
        return hashlib.md5(content.encode('utf-8')).hexdigest()
    
    def find_similar_chunks(self, query_embedding: np.ndarray, 
                          chunk_embeddings: np.ndarray, 
                          chunks: List[str], 
                          top_k: int = 5) -> List[Tuple[str, float]]:
        """Encontra chunks similares"""
        try:
            # Garantir dimensões corretas
            if query_embedding.ndim == 1:
                query_embedding = query_embedding.reshape(1, -1)
            
            # Calcular similaridade coseno
            query_norm = np.linalg.norm(query_embedding, axis=1, keepdims=True)
            chunks_norm = np.linalg.norm(chunk_embeddings, axis=1, keepdims=True)
            
            # Evitar divisão por zero
            query_norm = np.where(query_norm == 0, 1e-8, query_norm)
            chunks_norm = np.where(chunks_norm == 0, 1e-8, chunks_norm)
            
            # Normalizar
            query_normalized = query_embedding / query_norm
            chunks_normalized = chunk_embeddings / chunks_norm
            
            # Calcular similaridade
            similarities = np.dot(query_normalized, chunks_normalized.T).flatten()
            similarities = np.clip(similarities, -1.0, 1.0)
            
            # Top K
            top_k = min(top_k, len(similarities))
            top_indices = np.argsort(similarities)[-top_k:][::-1]
            
            results = []
            for idx in top_indices:
                if idx < len(chunks):
                    score = float(similarities[idx])
                    results.append((chunks[idx], score))
            
            return results
            
        except Exception as e:
            if self.verbose:
                print(f"⚠️ Erro na busca: {str(e)}")
            # Fallback
            import random
            available_chunks = list(enumerate(chunks))
            if len(available_chunks) > top_k:
                random_chunks = random.sample(available_chunks, top_k)
            else:
                random_chunks = available_chunks
            return [(chunks[idx], 0.5) for idx, _ in random_chunks]
    
    def get_status(self) -> Dict[str, str]:
        """Retorna status do gerenciador"""
        return {
            "mode": "offline" if self.offline_mode else "online",
            "model": self.current_model,
            "method": "HF Hub" if (self.client and not self.use_requests) else "Requests",
            "token_configured": bool(self.api_token),
            "hf_hub_available": HF_HUB_AVAILABLE,
            "cache_dir": str(self.cache_dir)
        }
    
    def test_connection(self) -> bool:
        """Testa conexão"""
        if self.offline_mode:
            return False
        
        try:
            if self.client and not self.use_requests:
                result = self.client.feature_extraction("test", model=self.current_model)
                return result is not None
            else:
                url = f"https://api-inference.huggingface.co/models/{self.current_model}"
                response = requests.post(
                    url,
                    headers=self.headers,
                    json={"inputs": "test"},
                    timeout=10
                )
                return response.status_code in [200, 503]
        except Exception:
            return False