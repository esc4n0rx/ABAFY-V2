# abapfy/agents/code_reviewer.py

import json
from typing import Dict, Any
from abapfy.agents.base_agent import BaseAgent
import re

class CodeReviewerAgent(BaseAgent):
    """Agente responsável pela revisão final do código"""
    
    def __init__(self, ai_client, config):
        super().__init__(ai_client, config, "code_reviewer")
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Executa revisão do código, garantindo que o código seja sempre retornado."""
        generated_code = input_data.get("generated_code", "")
        requirements = input_data.get("requirements", {})
        implementation_notes = input_data.get("implementation_notes", [])
        
        # Garantir que temos um código de entrada limpo e válido
        clean_code = self._ensure_clean_abap_code(generated_code)
        
        # Se o código de entrada já estiver vazio, não há o que revisar
        if not clean_code:
            return {
                "reviewed_code": "",
                "corrections_made": [],
                "quality_score": 0,
                "recommendations": ["Nenhum código gerado para revisar."]
            }

        # Construir contexto
        context = {
            "generated_code": clean_code,
            "requirements": json.dumps(requirements, indent=2),
            "implementation_notes": json.dumps(implementation_notes, indent=2)
        }
        
        # Gerar revisão
        review_prompt = self._build_prompt(context)
        
        try:
            response = self.ai_client._make_request(review_prompt)
            
            try:
                result = json.loads(response)
                
                # Verificar se o código revisado existe e é válido
                reviewed_code = result.get("reviewed_code", "")
                if reviewed_code and self._is_valid_abap(reviewed_code):
                    result["reviewed_code"] = self._ensure_clean_abap_code(reviewed_code)
                else:
                    # Se não houver código revisado válido, retorna o original
                    result["reviewed_code"] = clean_code
                    result.setdefault("recommendations", []).append(
                        "Código revisado não era válido ou estava ausente. Retornando código original."
                    )
                    
            except json.JSONDecodeError:
                # Se a resposta não for JSON, pode ser código puro ou um texto de análise
                potential_code = self._ensure_clean_abap_code(response)
                if self._is_valid_abap(potential_code):
                    # A resposta é um código ABAP válido
                    result = {
                        "reviewed_code": potential_code,
                        "corrections_made": [],
                        "quality_score": 80, # Score padrão para código puro
                        "recommendations": ["Revisão retornou código ABAP puro."]
                    }
                else:
                    # A resposta é um texto de análise, então usamos o código original
                    result = {
                        "reviewed_code": clean_code, # Fallback para o código original
                        "corrections_made": [],
                        "quality_score": 75, # Score menor indicando que a revisão pode ter falhado
                        "recommendations": [
                            "A resposta da revisão não continha código ABAP válido.",
                            f"Análise recebida: {response[:200]}..." # Log da análise
                        ]
                    }

            return result
            
        except Exception as e:
            # Em caso de erro na chamada da API, retornar o código original
            return {
                "reviewed_code": clean_code,
                "corrections_made": [],
                "quality_score": 70, # Score ainda menor para indicar falha
                "recommendations": [f"Erro crítico durante a revisão: {str(e)}. Código original preservado."]
            }

    def _is_valid_abap(self, code: str) -> bool:
        """Verificação simples se o texto parece ser código ABAP."""
        if not code or not isinstance(code, str):
            return False
        
        # Procura por palavras-chave comuns de início de programa/bloco ABAP
        abap_keywords = ["REPORT", "FUNCTION", "CLASS", "METHOD", "FORM", "START-OF-SELECTION", "*&---"]
        return any(keyword in code.upper() for keyword in abap_keywords)

    def _ensure_clean_abap_code(self, code: str) -> str:
        """Garante que o código está limpo e é puramente ABAP"""
        if not code or not isinstance(code, str) or not code.strip():
            return ""
        
        # Remover blocos de código markdown e JSON
        code = re.sub(r'```json.*?```', '', code, flags=re.DOTALL)
        code = re.sub(r'```abap\n?', '', code)
        code = re.sub(r'```', '', code)
        
        # Remove explicações comuns que podem vir junto com o código
        lines = code.split('\n')
        
        # Filtra linhas que não parecem ser código
        abap_lines = [
            line for line in lines 
            if not line.strip().startswith(("**Observações**", "- ", '"**Observações'))
        ]
        
        return '\n'.join(abap_lines).strip()