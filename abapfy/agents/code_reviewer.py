import json
from typing import Dict, Any
from abapfy.agents.base_agent import BaseAgent
import re

class CodeReviewerAgent(BaseAgent):
    """Agente responsável pela revisão final do código"""
    
    def __init__(self, ai_client, config):
        super().__init__(ai_client, config, "code_reviewer")
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Executa revisão do código"""
        generated_code = input_data.get("generated_code", "")
        requirements = input_data.get("requirements", {})
        implementation_notes = input_data.get("implementation_notes", [])
        
        # Garantir que temos código ABAP limpo
        clean_code = self._ensure_clean_abap_code(generated_code)
        
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
                
                # Garantir que o código revisado está limpo
                if result.get("reviewed_code"):
                    result["reviewed_code"] = self._ensure_clean_abap_code(result["reviewed_code"])
                else:
                    result["reviewed_code"] = clean_code
                    
            except json.JSONDecodeError:
                # Se não conseguir parsear JSON, assumir que é código puro
                reviewed_code = self._ensure_clean_abap_code(response)
                result = {
                    "reviewed_code": reviewed_code,
                    "corrections_made": [],
                    "quality_score": 80,
                    "recommendations": ["Código revisado e limpo"]
                }
            
            return result
            
        except Exception as e:
            # Em caso de erro, retornar código original limpo
            return {
                "reviewed_code": clean_code,
                "corrections_made": [],
                "quality_score": 75,
                "recommendations": [f"Erro na revisão: {str(e)}. Código original preservado."]
            }
    
    def _ensure_clean_abap_code(self, code: str) -> str:
        """Garante que o código está limpo e é puramente ABAP"""
        if not code.strip():
            return ""
        
        # Remover texto explicativo e JSON
        clean_code = code
        
        # Remover blocos JSON se presentes
        clean_code = re.sub(r'```json.*?```', '', clean_code, flags=re.DOTALL)
        
        # Remover texto introdutório
        intro_patterns = [
            r'^.*?(?=\*&-{5,})',  # Tudo antes do cabeçalho ABAP
            r'^.*?(?=REPORT\s+)',  # Tudo antes de REPORT
            r'^.*?(?=FUNCTION\s+)', # Tudo antes de FUNCTION
            r'^.*?(?=CLASS\s+\w+\s+DEFINITION)'  # Tudo antes de CLASS
        ]
        
        for pattern in intro_patterns:
            match = re.search(pattern, clean_code, re.DOTALL | re.IGNORECASE)
            if match:
                clean_code = clean_code[match.end():]
                break
        
        # Remover blocos de código markdown
        clean_code = re.sub(r'```\w*\n', '', clean_code)
        clean_code = re.sub(r'\n```', '', clean_code)
        
        # Remover linhas com JSON no final
        lines = clean_code.split('\n')
        abap_lines = []
        
        for line in lines:
            # Parar se encontrar JSON ou texto explicativo
            if any(pattern in line for pattern in ['"generated_code"', '"implementation_notes"', '{"', '},']):
                break
            abap_lines.append(line)
        
        return '\n'.join(abap_lines).strip()