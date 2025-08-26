# abapfy/analyzers/debugger.py
from typing import List, Dict, Any, Optional
from abapfy.embeddings.manager import EmbeddingManager
from abapfy.embeddings.chunker import ABAPCodeChunker, CodeChunk
from abapfy.ai.client import AIClient
from abapfy.config.manager import ConfigManager
from abapfy.utils.loading import LoadingContext, ProgressBar
import re

class ABAPDebugger:
    """Analisador de debug para código ABAP usando embeddings"""
    
    def __init__(self, config: ConfigManager):
        self.config = config
        self.ai_client = AIClient(config)
        self.embedding_manager = EmbeddingManager(verbose=False)  # Desabilitar logs verbosos
        self.chunker = ABAPCodeChunker()
        
        # Padrões específicos de erro em ABAP com exemplos mais detalhados
        self.error_patterns = {
            "runtime_errors": [
                "division by zero without check",
                "array index bounds validation missing",
                "null reference pointer not validated", 
                "SELECT without WHERE performance problem",
                "memory allocation error not handled",
                "LOOP AT internal table without validation",
                "MOVE-CORRESPONDING field mismatch risk",
                "CALL TRANSACTION without authorization"
            ],
            "syntax_logic_errors": [
                "missing ENDIF statement IF block",
                "missing ENDLOOP statement LOOP block", 
                "missing ENDFORM statement FORM routine",
                "missing ENDMETHOD statement METHOD block",
                "variable used before DATA declaration",
                "incorrect assignment TYPE vs LIKE",
                "unreachable code after RETURN EXIT",
                "infinite loop condition never updated"
            ],
            "abap_specific_errors": [
                "SELECT * FROM table bad performance",
                "sy-subrc not checked database operation",
                "hardcoded literal values magic numbers",
                "missing TRY CATCH exception handling",
                "wrong TYPE declaration LIKE misuse",
                "missing AUTHORITY-CHECK security risk",
                "COMMIT WORK without error handling",
                "internal table no proper SORT key"
            ],
            "performance_issues": [
                "nested SELECT inside LOOP performance",
                "database access in loop iteration", 
                "internal table operations no index",
                "LOOP AT WHERE large table scan",
                "missing database hints optimization",
                "unnecessary MOVE operations copying",
                "string concatenation inefficient method"
            ]
        }
        
        # Padrões de regex para detectar problemas diretamente no código
        self.regex_patterns = {
            "division_by_zero": r'\/\s*[a-zA-Z_]\w*(?!\s*[!=<>])',
            "select_star": r'SELECT\s+\*\s+FROM\s+\w+',
            "missing_where": r'SELECT\s+(?:(?!\*)[^F])+FROM\s+\w+(?!\s+WHERE)',
            "sy_subrc_not_checked": r'SELECT\s+.*?\.\s*\n(?!\s*IF\s+sy-subrc)',
            "missing_authority_check": r'SELECT\s+.*FROM\s+(vbak|kna1|mara)(?!\s*.*AUTHORITY-CHECK)',
            "hardcoded_values": r"'[^']{3,}'(?!\s*(TYPE|LIKE))",
            "missing_exception": r'(CALL\s+FUNCTION|CALL\s+METHOD).*?(?!\s*EXCEPTIONS)',
            "infinite_loop": r'WHILE\s+.*(?!\s*.*(?:ADD|SUBTRACT|\+|\-|\=))',
            "missing_endif": r'IF\s+.*?\.(?!\s*.*ENDIF)',
            "missing_initialization": r'DATA:\s*(\w+).*?\.\s*(?!\s*\1\s*=)'
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
            
            with LoadingContext("Analisando código para debug", "spinner") as loader:
                # Análise direta por regex (mais eficiente)
                regex_issues = self._analyze_with_regex(code_content)
                
                # Dividir código em chunks
                chunks = self.chunker.chunk_code(code_content)
                
                # Criar embeddings apenas se necessário
                chunk_texts = [chunk.content for chunk in chunks]
                content_hash = self.embedding_manager.calculate_content_hash(code_content)
                
                # Verificar cache
                chunk_embeddings = self.embedding_manager.get_cached_embeddings(content_hash)
                if chunk_embeddings is None:
                    # Progress callback para embeddings
                    def progress_callback(current, total, status):
                        pass  # Silencioso para não poluir logs
                    
                    chunk_embeddings = self.embedding_manager.create_embeddings(
                        chunk_texts, progress_callback
                    )
                    self.embedding_manager.cache_embeddings(content_hash, chunk_embeddings)
                
                # Análise baseada em padrões de erro
                embedding_analysis = self._analyze_error_patterns(chunk_texts, chunk_embeddings)
                
                # Análise contextual se erro foi fornecido
                contextual_analysis = {}
                if error_description:
                    contextual_analysis = self._analyze_specific_error(
                        error_description, error_location, chunks, chunk_embeddings, chunk_texts
                    )
                
                # Combinar análises
                combined_analysis = self._combine_analyses(regex_issues, embedding_analysis)
                
                # Gerar análise detalhada com IA apenas para chunks mais problemáticos
                detailed_debug = self._generate_detailed_debug_analysis(
                    chunks, combined_analysis, contextual_analysis, error_description
                )
            
            # Criar relatório de debug
            debug_report = self._create_debug_report(
                file_path, chunks, combined_analysis, contextual_analysis, 
                detailed_debug, error_description, error_location, regex_issues
            )
            
            return debug_report
            
        except Exception as e:
            raise RuntimeError(f"Erro na análise de debug: {str(e)}")
    
    def _analyze_with_regex(self, code_content: str) -> Dict[str, List[Dict]]:
        """Análise direta por regex - mais eficiente para padrões conhecidos"""
        issues = {}
        lines = code_content.split('\n')
        
        for pattern_name, regex_pattern in self.regex_patterns.items():
            matches = []
            
            # Analisar linha por linha
            for line_num, line in enumerate(lines, 1):
                try:
                    if re.search(regex_pattern, line, re.IGNORECASE):
                        matches.append({
                            'line': line_num,
                            'content': line.strip(),
                            'severity': self._get_severity_for_pattern(pattern_name),
                            'description': self._get_description_for_pattern(pattern_name)
                        })
                except re.error:
                    continue
            
            if matches:
                issues[pattern_name] = matches
        
        return issues
    
    def _get_severity_for_pattern(self, pattern_name: str) -> str:
        """Retorna severidade baseada no padrão"""
        critical_patterns = ['division_by_zero', 'missing_authority_check', 'sy_subrc_not_checked']
        high_patterns = ['select_star', 'missing_exception', 'infinite_loop']
        
        if pattern_name in critical_patterns:
            return "CRITICAL"
        elif pattern_name in high_patterns:
            return "HIGH"
        else:
            return "MEDIUM"
    
    def _get_description_for_pattern(self, pattern_name: str) -> str:
        """Retorna descrição do problema"""
        descriptions = {
            'division_by_zero': 'Possível divisão por zero sem validação',
            'select_star': 'SELECT * detectado - má prática de performance',
            'missing_where': 'SELECT sem WHERE - risco de performance',
            'sy_subrc_not_checked': 'sy-subrc não verificado após operação de banco',
            'missing_authority_check': 'Falta verificação de autorização',
            'hardcoded_values': 'Valores hardcoded encontrados',
            'missing_exception': 'Tratamento de exceção ausente',
            'infinite_loop': 'Possível loop infinito detectado',
            'missing_endif': 'Estrutura IF sem ENDIF correspondente',
            'missing_initialization': 'Variável possivelmente usada sem inicialização'
        }
        return descriptions.get(pattern_name, 'Problema detectado por análise regex')
    
    def _combine_analyses(self, regex_issues: Dict, embedding_analysis: Dict) -> Dict[str, Any]:
        """Combina análises de regex e embedding"""
        combined = {
            'regex_issues': regex_issues,
            'embedding_issues': embedding_analysis,
            'total_critical': 0,
            'total_high': 0,
            'total_medium': 0
        }
        
        # Contar severidades
        for pattern_issues in regex_issues.values():
            for issue in pattern_issues:
                severity = issue['severity']
                if severity == 'CRITICAL':
                    combined['total_critical'] += 1
                elif severity == 'HIGH':
                    combined['total_high'] += 1
                else:
                    combined['total_medium'] += 1
        
        return combined
    
    def _analyze_error_patterns(self, chunk_texts: List[str], 
                               chunk_embeddings) -> Dict[str, Any]:
        """Analisa código procurando padrões de erro conhecidos (usando embeddings)"""
        pattern_analysis = {}
        
        for error_type, patterns in self.error_patterns.items():
            # Criar embedding para padrões deste tipo de erro
            pattern_embedding = self.embedding_manager.create_embeddings(patterns)[0]
            
            # Encontrar chunks similares
            similar_chunks = self.embedding_manager.find_similar_chunks(
                pattern_embedding, chunk_embeddings, chunk_texts, top_k=3
            )
            
            # Filtrar apenas chunks com similaridade significativa
            significant_chunks = [(chunk, score) for chunk, score in similar_chunks if score > 0.3]
            
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
                                        combined_analysis: Dict, 
                                        contextual_analysis: Dict,
                                        error_description: Optional[str]) -> Dict[str, str]:
        """Gera análise detalhada usando IA apenas para casos críticos"""
        detailed_analysis = {}
        
        # Verificar se há problemas críticos encontrados por regex
        regex_issues = combined_analysis.get('regex_issues', {})
        critical_chunks = set()
        
        # Identificar chunks com problemas críticos
        for pattern_name, issues in regex_issues.items():
            if issues and len(issues) > 0:
                for issue in issues:
                    if issue['severity'] in ['CRITICAL', 'HIGH']:
                        # Encontrar chunk que contém essa linha
                        target_line = issue['line']
                        for chunk in chunks:
                            if chunk.start_line <= target_line <= chunk.end_line:
                                critical_chunks.add(chunk.content)
                                break
        
        # Adicionar chunks da análise contextual se existir
        if contextual_analysis.get('related_chunks'):
            for chunk_content, score in contextual_analysis['related_chunks']:
                if score > 0.6:  # Alta relevância
                    critical_chunks.add(chunk_content)
        
        # Analisar apenas chunks críticos com IA (máximo 3 para performance)
        for chunk_content in list(critical_chunks)[:3]:
            try:
                # Construir contexto específico
                context_parts = []
                if error_description:
                    context_parts.append(f"Erro relatado: {error_description}")
                
                # Adicionar problemas de regex encontrados neste chunk
                for pattern_name, issues in regex_issues.items():
                    chunk_lines = chunk_content.split('\n')
                    for issue in issues:
                        if any(issue['content'] in line for line in chunk_lines):
                            context_parts.append(f"Problema detectado: {issue['description']}")
                
                debug_context = "\n".join(context_parts) if context_parts else "Análise geral de debug"
                
                chunk_debug = self.ai_client.debug_code([chunk_content], debug_context)
                detailed_analysis[f"chunk_{len(detailed_analysis) + 1}"] = chunk_debug
                
            except Exception:
                continue
        
        return detailed_analysis
    
    def _create_debug_report(self, file_path: str, chunks: List[CodeChunk],
                           combined_analysis: Dict, contextual_analysis: Dict,
                           detailed_debug: Dict, error_description: Optional[str],
                           error_location: Optional[str], regex_issues: Dict) -> Dict[str, Any]:
        """Cria relatório consolidado de debug"""
        
        # Calcular score de problemas baseado em análise regex + embedding
        total_critical = combined_analysis.get('total_critical', 0)
        total_high = combined_analysis.get('total_high', 0) 
        total_medium = combined_analysis.get('total_medium', 0)
        
        # Problemas de embedding
        embedding_issues = sum(
            analysis['risk_level'] for analysis in combined_analysis.get('embedding_issues', {}).values()
        )
        
        total_issues = total_critical + total_high + total_medium + embedding_issues
        
        confidence_score = contextual_analysis.get('confidence_score', 0) * 100
        
        # Determinar prioridade de debug baseada em problemas reais encontrados
        if total_critical > 0:
            debug_priority = f"🔴 CRÍTICA - {total_critical} problemas críticos encontrados"
        elif total_high > 0:
            debug_priority = f"🟠 ALTA - {total_high} problemas de alta prioridade"
        elif total_medium > 2:
            debug_priority = f"🟡 MÉDIA - {total_medium} problemas detectados"
        elif confidence_score > 60:
            debug_priority = "🔵 BAIXA - Erro específico localizado"
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
                "total_issues_found": total_issues,
                "critical_issues": total_critical,
                "high_issues": total_high,
                "medium_issues": total_medium,
                "most_likely_cause": self._determine_likely_cause(combined_analysis, contextual_analysis)
            },
            "regex_analysis": regex_issues,
            "embedding_analysis": combined_analysis.get('embedding_issues', {}),
            "contextual_analysis": contextual_analysis,
            "detailed_debug_analysis": detailed_debug,
            "debug_suggestions": self._generate_debug_suggestions(combined_analysis, contextual_analysis),
            "next_steps": self._generate_next_steps(combined_analysis, contextual_analysis, error_description)
        }
        
        return report
    
    def _determine_likely_cause(self, combined_analysis: Dict, contextual_analysis: Dict) -> str:
        """Determina a causa mais provável baseada na análise"""
        regex_issues = combined_analysis.get('regex_issues', {})
        
        # Priorizar problemas críticos encontrados por regex
        critical_patterns = []
        for pattern_name, issues in regex_issues.items():
            if issues and any(issue['severity'] == 'CRITICAL' for issue in issues):
                critical_patterns.append(pattern_name)
        
        if critical_patterns:
            if 'division_by_zero' in critical_patterns:
                return "Divisão por zero sem validação detectada"
            elif 'missing_authority_check' in critical_patterns:
                return "Falta verificação de autorização em acesso crítico"
            elif 'sy_subrc_not_checked' in critical_patterns:
                return "Operações de banco sem verificação de sy-subrc"
            else:
                return f"Problemas críticos: {', '.join(critical_patterns)}"
        
        if contextual_analysis.get('confidence_score', 0) > 0.6:
            return "Erro específico localizado com alta confiança"
        
        # Verificar problemas de alto impacto
        high_patterns = []
        for pattern_name, issues in regex_issues.items():
            if issues and any(issue['severity'] == 'HIGH' for issue in issues):
                high_patterns.append(pattern_name)
        
        if high_patterns:
            return f"Problemas de alta prioridade: {', '.join(high_patterns)}"
        
        if combined_analysis.get('total_medium', 0) > 0:
            return f"{combined_analysis['total_medium']} problemas de média prioridade detectados"
        
        return "Código com boa qualidade - verifique dados de entrada e lógica de negócio"
    
    def _generate_debug_suggestions(self, combined_analysis: Dict, contextual_analysis: Dict) -> List[str]:
        """Gera sugestões específicas para debug"""
        suggestions = []
        regex_issues = combined_analysis.get('regex_issues', {})
        
        # Sugestões baseadas em problemas específicos encontrados
        if regex_issues.get('division_by_zero'):
            suggestions.append("🔢 Adicione validação antes de operações de divisão (IF divisor <> 0)")
        
        if regex_issues.get('select_star'):
            suggestions.append("📊 Substitua SELECT * por campos específicos para melhorar performance")
        
        if regex_issues.get('sy_subrc_not_checked'):
            suggestions.append("✅ Sempre verifique sy-subrc após operações de banco de dados")
        
        if regex_issues.get('missing_authority_check'):
            suggestions.append("🔒 Implemente AUTHORITY-CHECK antes de acessar dados sensíveis")
        
        if regex_issues.get('missing_exception'):
            suggestions.append("⚠️ Adicione tratamento de exceções (TRY...CATCH ou EXCEPTIONS)")
        
        if regex_issues.get('hardcoded_values'):
            suggestions.append("📝 Substitua valores hardcoded por constantes ou parâmetros")
        
        if contextual_analysis.get('confidence_score', 0) > 0.5:
            suggestions.append("🎯 Foque na análise detalhada dos chunks identificados")
            suggestions.append("🔧 Execute debug passo-a-passo na área suspeita")
        
        if not suggestions:
            suggestions.append("✅ Execute testes unitários para validar comportamento")
            suggestions.append("📊 Adicione logs detalhados para rastreamento")
        
        return suggestions
    
    def _generate_next_steps(self, combined_analysis: Dict, contextual_analysis: Dict,
                            error_description: Optional[str]) -> List[str]:
        """Gera próximos passos para resolução"""
        next_steps = []
        
        if combined_analysis.get('total_critical', 0) > 0:
            next_steps.append("1. 🚨 PRIORIDADE: Corrija problemas críticos identificados")
            next_steps.append("2. 🧪 Teste cada correção individual antes de prosseguir")
        elif error_description:
            next_steps.append("1. 🔍 Execute o código com debugger na área identificada")
            next_steps.append("2. 📝 Valide valores de variáveis nos pontos suspeitos")
        else:
            next_steps.append("1. 🧪 Execute testes com casos extremos")
            next_steps.append("2. 📊 Adicione logs em pontos críticos")
        
        next_steps.append("3. 📋 Implemente melhorias sugeridas na análise")
        next_steps.append("4. ✅ Execute bateria de testes após correções")
        next_steps.append("5. 📖 Documente as correções implementadas")
        
        return next_steps
    
    def _get_timestamp(self) -> str:
        """Obtém timestamp atual"""
        from datetime import datetime
        return datetime.now().strftime("%Y-%m-%d %H:%M:%S")