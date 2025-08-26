# abapfy/analyzers/debugger.py
from typing import List, Dict, Any, Optional
from abapfy.embeddings.manager import EmbeddingManager
from abapfy.embeddings.chunker import ABAPCodeChunker, CodeChunk
from abapfy.ai.client import AIClient
from abapfy.config.manager import ConfigManager

class ABAPDebugger:
    """Analisador de debug para código ABAP usando embeddings"""
    
    def __init__(self, config: ConfigManager):
        self.config = config
        self.ai_client = AIClient(config)
        self.embedding_manager = EmbeddingManager()
        self.chunker = ABAPCodeChunker()
        
        # Padrões comuns de erro em ABAP
        self.error_patterns = {
            "runtime_errors": [
                "division by zero",
                "array index out of bounds", 
                "null pointer reference",
                "type conversion error",
                "memory allocation failed"
            ],
            "syntax_errors": [
                "missing ENDIF statement",
                "unmatched parentheses",
                "undefined variable reference",
                "invalid field assignment",
                "missing ENDLOOP statement"
            ],
            "logic_errors": [
                "infinite loop condition",
                "wrong variable assignment",
                "incorrect condition logic",
                "missing initialization",
                "unreachable code block"
            ],
            "database_errors": [
                "table not found",
                "field not found in table",
                "authorization error database",
                "transaction deadlock",
                "connection timeout"
            ]
        }
    
    def analyze_code_for_debug(self, file_path: str, 
                              error_description: Optional[str] = None,
                              error_location: Optional[str] = None) -> Dict[str, Any]:
        """Analisa código ABAP para debug com contexto do erro"""
        try:
            # Ler arquivo
            with open(file_path, 'r', encoding='utf-8') as f:
                code_content = f.read()
            
            if not code_content.strip():
                raise ValueError("Arquivo está vazio")
            
            # Dividir código em chunks
            chunks = self.chunker.chunk_code(code_content)
            
            # Criar embeddings
            chunk_texts = [chunk.content for chunk in chunks]
            content_hash = self.embedding_manager.calculate_content_hash(code_content)
            
            # Verificar cache
            chunk_embeddings = self.embedding_manager.get_cached_embeddings(content_hash)
            if chunk_embeddings is None:
                chunk_embeddings = self.embedding_manager.create_embeddings(chunk_texts)
                self.embedding_manager.cache_embeddings(content_hash, chunk_embeddings)
            
            # Análise baseada em padrões de erro
            error_analysis = self._analyze_error_patterns(chunk_texts, chunk_embeddings)
            
            # Análise contextual se erro foi fornecido
            contextual_analysis = {}
            if error_description:
                contextual_analysis = self._analyze_specific_error(
                    error_description, error_location, chunks, chunk_embeddings, chunk_texts
                )
            
            # Gerar análise detalhada com IA
            detailed_debug = self._generate_detailed_debug_analysis(
                chunks, error_analysis, contextual_analysis, error_description
            )
            
            # Criar relatório de debug
            debug_report = self._create_debug_report(
                file_path, chunks, error_analysis, contextual_analysis, 
                detailed_debug, error_description, error_location
            )
            
            return debug_report
            
        except Exception as e:
            raise RuntimeError(f"Erro na análise de debug: {str(e)}")
    
    def _analyze_error_patterns(self, chunk_texts: List[str], 
                               chunk_embeddings) -> Dict[str, Any]:
        """Analisa código procurando padrões de erro conhecidos"""
        pattern_analysis = {}
        
        for error_type, patterns in self.error_patterns.items():
            # Criar embedding para padrões deste tipo de erro
            pattern_embedding = self.embedding_manager.create_embeddings(patterns)[0]
            
            # Encontrar chunks similares
            similar_chunks = self.embedding_manager.find_similar_chunks(
                pattern_embedding, chunk_embeddings, chunk_texts, top_k=5
            )
            
            # Filtrar apenas chunks com similaridade significativa
            significant_chunks = [(chunk, score) for chunk, score in similar_chunks if score > 0.25]
            
            pattern_analysis[error_type] = {
                'potential_issues': significant_chunks,
                'risk_level': len(significant_chunks)
            }
        
        return pattern_analysis
    
    def _analyze_specific_error(self, error_description: str, error_location: Optional[str],
                               chunks: List[CodeChunk], chunk_embeddings, 
                               chunk_texts: List[str]) -> Dict[str, Any]:
        """Analisa código focando no erro específico relatado"""
        # Criar embedding da descrição do erro
        error_embedding = self.embedding_manager.create_embeddings([error_description])[0]
        
        # Encontrar chunks mais relacionados ao erro
        related_chunks = self.embedding_manager.find_similar_chunks(
            error_embedding, chunk_embeddings, chunk_texts, top_k=3
        )
        
        # Se localização foi fornecida, dar prioridade a chunks naquela região
        if error_location:
            try:
                # Extrair número da linha se fornecido
                line_numbers = [int(s) for s in error_location.split() if s.isdigit()]
                if line_numbers:
                    target_line = line_numbers[0]
                    
                    # Encontrar chunk que contém a linha
                    target_chunks = []
                    for i, chunk in enumerate(chunks):
                        if chunk.start_line <= target_line <= chunk.end_line:
                            target_chunks.append((chunk_texts[i], 1.0))  # Máxima relevância
                            break
                    
                    if target_chunks:
                        related_chunks = target_chunks + related_chunks
            except:
                pass
        
        return {
            'error_description': error_description,
            'error_location': error_location,
            'related_chunks': related_chunks,
            'confidence_score': max([score for _, score in related_chunks]) if related_chunks else 0
        }
    
    def _generate_detailed_debug_analysis(self, chunks: List[CodeChunk], 
                                        error_analysis: Dict, 
                                        contextual_analysis: Dict,
                                        error_description: Optional[str]) -> Dict[str, str]:
        """Gera análise detalhada usando IA"""
        detailed_analysis = {}
        
        # Coletar chunks mais problemáticos
        problematic_chunks = set()
        
        # Adicionar chunks da análise de padrões
        for error_type, analysis in error_analysis.items():
            for chunk_content, score in analysis['potential_issues']:
                if score > 0.3:
                    problematic_chunks.add(chunk_content)
        
        # Adicionar chunks da análise contextual
        if contextual_analysis.get('related_chunks'):
            for chunk_content, score in contextual_analysis['related_chunks']:
                if score > 0.4:
                    problematic_chunks.add(chunk_content)
        
        # Analisar chunks mais relevantes com IA
        for chunk_content in list(problematic_chunks)[:3]:  # Limitar a 3 chunks
            try:
                # Criar contexto específico para debug
                debug_context = f"""
CONTEXTO DO DEBUG:
- Erro reportado: {error_description or 'Análise geral de debug'}
- Chunk analisado: {chunk_content[:200]}...

Analise este código ABAP buscando:
1. Possíveis causas do erro
2. Pontos de falha
3. Variáveis não inicializadas
4. Lógica incorreta
5. Sugestões de correção
"""
                
                chunk_debug = self.ai_client.debug_code(chunk_content + "\n\n" + debug_context)
                detailed_analysis[chunk_content[:100] + "..."] = chunk_debug
                
            except Exception:
                continue
        
        return detailed_analysis
    
    def _create_debug_report(self, file_path: str, chunks: List[CodeChunk],
                           error_analysis: Dict, contextual_analysis: Dict,
                           detailed_debug: Dict, error_description: Optional[str],
                           error_location: Optional[str]) -> Dict[str, Any]:
        """Cria relatório consolidado de debug"""
        
        # Calcular score de problemas potenciais
        total_issues = sum(
            analysis['risk_level'] for analysis in error_analysis.values()
        )
        
        confidence_score = contextual_analysis.get('confidence_score', 0) * 100
        
        # Determinar prioridade de debug
        if confidence_score > 70:
            debug_priority = "🔴 ALTA - Erro localizado"
        elif confidence_score > 40:
            debug_priority = "🟠 MÉDIA - Área suspeita identificada"
        elif total_issues > 5:
            debug_priority = "🟡 BAIXA - Múltiplos pontos de atenção"
        else:
            debug_priority = "🟢 BAIXA - Poucos problemas detectados"
        
        report = {
            "metadata": {
                "file_path": file_path,
                "error_description": error_description,
                "error_location": error_location,
                "total_lines": sum(chunk.end_line - chunk.start_line + 1 for chunk in chunks),
                "total_chunks": len(chunks),
                "analysis_timestamp": self._get_timestamp()
            },
            "debug_summary": {
                "debug_priority": debug_priority,
                "confidence_score": round(confidence_score, 2),
                "potential_issues_found": total_issues,
                "most_likely_cause": self._determine_likely_cause(error_analysis, contextual_analysis)
            },
            "error_pattern_analysis": error_analysis,
            "contextual_analysis": contextual_analysis,
            "detailed_debug_analysis": detailed_debug,
            "debug_suggestions": self._generate_debug_suggestions(error_analysis, contextual_analysis),
            "next_steps": self._generate_next_steps(error_analysis, contextual_analysis, error_description)
        }
        
        return report
    
    def _determine_likely_cause(self, error_analysis: Dict, contextual_analysis: Dict) -> str:
        """Determina a causa mais provável baseada na análise"""
        if contextual_analysis.get('confidence_score', 0) > 0.6:
            return "Erro específico localizado com alta confiança"
        
        # Encontrar tipo de erro com mais ocorrências
        max_risk = 0
        likely_cause = "Não determinado"
        
        for error_type, analysis in error_analysis.items():
            if analysis['risk_level'] > max_risk:
                max_risk = analysis['risk_level']
                likely_cause = error_type.replace('_', ' ').title()
        
        if max_risk > 0:
            return f"Possível {likely_cause}"
        
        return "Código aparenta estar correto - verificar dados de entrada"
    
    def _generate_debug_suggestions(self, error_analysis: Dict, contextual_analysis: Dict) -> List[str]:
        """Gera sugestões específicas para debug"""
        suggestions = []
        
        # Sugestões baseadas em padrões detectados
        for error_type, analysis in error_analysis.items():
            if analysis['risk_level'] > 0:
                if error_type == "runtime_errors":
                    suggestions.append("🔍 Adicione verificações de divisão por zero e validação de arrays")
                elif error_type == "syntax_errors":
                    suggestions.append("📝 Verifique estruturas de controle (IF/ENDIF, LOOP/ENDLOOP)")
                elif error_type == "logic_errors":
                    suggestions.append("🧠 Revise condições de loop e inicialização de variáveis")
                elif error_type == "database_errors":
                    suggestions.append("💾 Verifique permissões e existência de tabelas/campos")
        
        if contextual_analysis.get('confidence_score', 0) > 0.5:
            suggestions.append("🎯 Foque na análise detalhada dos chunks identificados")
            suggestions.append("🔧 Execute debug passo-a-passo na área suspeita")
        
        if not suggestions:
            suggestions.append("✅ Execute testes unitários para validar comportamento")
            suggestions.append("📊 Adicione logs detalhados para rastreamento")
        
        return suggestions
    
    def _generate_next_steps(self, error_analysis: Dict, contextual_analysis: Dict,
                            error_description: Optional[str]) -> List[str]:
        """Gera próximos passos para resolução"""
        next_steps = []
        
        if error_description:
            next_steps.append("1. 🔍 Execute o código com debugger na área identificada")
            next_steps.append("2. 📝 Valide valores de variáveis nos pontos suspeitos")
            next_steps.append("3. 🧪 Teste com dados diferentes para reproduzir o erro")
        else:
            next_steps.append("1. 🧪 Execute testes com casos extremos")
            next_steps.append("2. 📊 Adicione logs em pontos críticos")
            next_steps.append("3. 🔍 Monitore performance e uso de memória")
        
        next_steps.append("4. 📋 Implemente melhorias sugeridas na análise")
        next_steps.append("5. ✅ Execute bateria de testes após correções")
        
        return next_steps
    
    def _get_timestamp(self) -> str:
        """Obtém timestamp atual"""
        from datetime import datetime
        return datetime.now().strftime("%Y-%m-%d %H:%M:%S")