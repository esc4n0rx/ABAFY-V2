import numpy as np
from sentence_transformers import SentenceTransformer
from typing import List, Dict, Tuple, Optional
import pickle
from pathlib import Path
import hashlib

class EmbeddingManager:
    """Gerenciador de embeddings para código ABAP"""
    
    def __init__(self, model_name: str = "all-MiniLM-L6-v2"):
        self.model = SentenceTransformer(model_name)
        self.cache_dir = Path.home() / ".abapfy" / "embeddings_cache"
        self.cache_dir.mkdir(parents=True, exist_ok=True)
    
    def create_embeddings(self, chunks: List[str]) -> np.ndarray:
        """Cria embeddings para lista de chunks"""
        try:
            embeddings = self.model.encode(chunks, convert_to_numpy=True)
            return embeddings
        except Exception as e:
            raise RuntimeError(f"Erro ao criar embeddings: {str(e)}")
    
    def get_cached_embeddings(self, content_hash: str) -> Optional[np.ndarray]:
        """Obtém embeddings do cache"""
        cache_file = self.cache_dir / f"{content_hash}.pkl"
        if cache_file.exists():
            try:
                with open(cache_file, 'rb') as f:
                    return pickle.load(f)
            except Exception:
                return None
        return None
    
    def cache_embeddings(self, content_hash: str, embeddings: np.ndarray):
        """Salva embeddings no cache"""
        cache_file = self.cache_dir / f"{content_hash}.pkl"
        try:
            with open(cache_file, 'wb') as f:
                pickle.dump(embeddings, f)
        except Exception:
            pass  # Cache failure is not critical
    
    def calculate_content_hash(self, content: str) -> str:
        """Calcula hash do conteúdo para cache"""
        return hashlib.md5(content.encode('utf-8')).hexdigest()
    
    def find_similar_chunks(self, query_embedding: np.ndarray, 
                          chunk_embeddings: np.ndarray, 
                          chunks: List[str], 
                          top_k: int = 5) -> List[Tuple[str, float]]:
        """Encontra chunks mais similares baseado em embeddings"""
        try:
            # Calcular similaridade coseno
            similarities = np.dot(query_embedding, chunk_embeddings.T)
            
            # Obter top K mais similares
            top_indices = np.argsort(similarities)[-top_k:][::-1]
            
            results = []
            for idx in top_indices:
                if idx < len(chunks):
                    results.append((chunks[idx], float(similarities[idx])))
            
            return results
        except Exception as e:
            raise RuntimeError(f"Erro ao buscar chunks similares: {str(e)}")