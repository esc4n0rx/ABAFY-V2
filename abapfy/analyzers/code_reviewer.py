# abapfy/analyzers/code_reviewer.py
from typing import List, Dict, Any
from abapfy.embeddings.manager import EmbeddingManager
from abapfy.embeddings.chunker import ABAPCodeChunker, CodeChunk
from abapfy.ai.client import AIClient
from abapfy.config.manager import ConfigManager
import json

class CodeReviewer:
    """Analisador de code review para código ABAP usando embeddings"""
    
    def __init__(self, config: ConfigManager):
        self.config = config
        self.ai_client = AIClient(config)
        self.embedding_manager = EmbeddingManager()
        self.chunker = ABAPCodeChunker()
        
        # Critérios de análise com embeddings de referência
        self.review_criteria = {
            "performance": [
                "SELECT * FROM table",
                "nested loops with database access",
                "inefficient internal table operations",
                "missing WHERE conditions in SELECT"
            ],
            "security": [
                "SQL injection vulnerability",
                "missing authorization check",
                "hardcoded credentials",
                "unvalidated user input"
            ],
            "best_practices": [
                "missing exception handling",
                "long methods without modularization", 
                "missing documentation",
                "obsolete ABAP syntax"
            ],
            "maintainability": [
                "complex nested conditions",
                "magic numbers without constants",
                "poor variable naming",
                "tight coupling between components"
            ]
        }
    
    def analyze_code(self, file_path: str) -> Dict[str, Any]:
        """Analisa código ABAP e gera relatório de review"""
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
            
            # Analisar cada critério
            analysis_results = {}
            
            for criterion, examples in self.review_criteria.items():
                criterion_embedding = self.embedding_manager.create_embeddings(examples)[0]
                similar_chunks = self.embedding_manager.find_similar_chunks(
                    criterion_embedding, chunk_embeddings, chunk_texts, top_k=3
                )
                
                analysis_results[criterion] = {
                    'suspicious_chunks': similar_chunks,
                    'issues_found': len([chunk for chunk, score in similar_chunks if score > 0.3])
                }
            
            # Gerar análise detalhada com IA para chunks problemáticos
            detailed_analysis = self._generate_detailed_analysis(chunks, analysis_results)
            
            # Criar relatório final
            report = self._create_review_report(file_path, chunks, analysis_results, detailed_analysis)
            
            return report
            
        except Exception as e:
            raise RuntimeError(f"Erro na análise de code review: {str(e)}")
    
    def _generate_detailed_analysis(self, chunks: List[CodeChunk], 
                                  analysis_results: Dict) -> Dict[str, Any]:
        """Gera análise detalhada usando IA para chunks problemáticos"""
        detailed_analysis = {}
        
        # Identificar chunks mais problemáticos
        problematic_chunks = set()
        for criterion, results in analysis_results.items():
            for chunk_content, score in results['suspicious_chunks']:
                if score > 0.4:  # Threshold para análise detalhada
                    problematic_chunks.add(chunk_content)
        
        # Analisar cada chunk problemático
        for chunk_content in list(problematic_chunks)[:5]:  # Limitar a 5 chunks
            try:
                chunk_analysis = self.ai_client.review_code(chunk_content)
                detailed_analysis[chunk_content[:100]] = chunk_analysis
            except Exception:
                continue
        
        return detailed_analysis
    
    def _create_review_report(self, file_path: str, chunks: List[CodeChunk], 
                            analysis_results: Dict, detailed_analysis: Dict) -> Dict[str, Any]:
        """Cria relatório consolidado de code review"""
        
        total_issues = sum(
            results['issues_found'] for results in analysis_results.values()
        )
        
        severity_score = min(total_issues * 10, 100)  # Score de 0-100
        
        if severity_score <= 20:
            overall_rating = "🟢 EXCELENTE"
        elif severity_score <= 50:
            overall_rating = "🟡 BOM"
        elif severity_score <= 80:
            overall_rating = "🟠 ATENÇÃO"
        else:
            overall_rating = "🔴 CRÍTICO"
        
        report = {
            "metadata": {
                "file_path": file_path,
                "total_lines": sum(chunk.end_line - chunk.start_line + 1 for chunk in chunks),
                "total_chunks": len(chunks),
                "analysis_timestamp": self._get_timestamp()
            },
            "summary": {
                "overall_rating": overall_rating,
                "severity_score": severity_score,
                "total_issues_found": total_issues,
                "chunks_analyzed": len(chunks)
            },
            "analysis_by_category": analysis_results,
            "detailed_findings": detailed_analysis,
            "recommendations": self._generate_recommendations(analysis_results),
            "chunk_overview": [
                {
                    "type": chunk.chunk_type,
                    "lines": f"{chunk.start_line}-{chunk.end_line}",
                    "context": chunk.context
                }
                for chunk in chunks
            ]
        }
        
        return report
    
    def _generate_recommendations(self, analysis_results: Dict) -> List[str]:
        """Gera recomendações baseadas na análise"""
        recommendations = []
        
        for criterion, results in analysis_results.items():
            if results['issues_found'] > 0:
                if criterion == "performance":
                    recommendations.append(
                        "🚀 Otimize consultas ao banco de dados usando campos específicos no SELECT"
                    )
                elif criterion == "security":
                    recommendations.append(
                        "🔒 Implemente verificações de autorização e valide entradas do usuário"
                    )
                elif criterion == "best_practices":
                    recommendations.append(
                        "📋 Adicione tratamento de exceções e documentação adequada"
                    )
                elif criterion == "maintainability":
                    recommendations.append(
                        "🔧 Refatore métodos longos e melhore nomenclatura de variáveis"
                    )
        
        if not recommendations:
            recommendations.append("✅ Código está seguindo boas práticas!")
        
        return recommendations
    
    def _get_timestamp(self) -> str:
        """Obtém timestamp atual"""
        from datetime import datetime
        return datetime.now().strftime("%Y-%m-%d %H:%M:%S")