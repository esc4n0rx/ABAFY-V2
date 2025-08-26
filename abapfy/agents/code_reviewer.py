import json
from typing import Dict, Any
from abapfy.agents.base_agent import BaseAgent

class CodeReviewerAgent(BaseAgent):
    """Agente responsável pela revisão final do código"""
    
    def __init__(self, ai_client, config):
        super().__init__(ai_client, config, "code_reviewer")
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Executa revisão do código"""
        generated_code = input_data.get("generated_code", "")
        requirements = input_data.get("requirements", {})
        implementation_notes = input_data.get("implementation_notes", [])
        
        # Construir contexto
        context = {
            "generated_code": generated_code,
            "requirements": json.dumps(requirements, indent=2),
            "implementation_notes": json.dumps(implementation_notes, indent=2)
        }
        
        # Gerar revisão
        review_prompt = self._build_prompt(context)
        
        try:
            response = self.ai_client._make_request(review_prompt)
            
            try:
                result = json.loads(response)
                # Garantir que o código revisado existe
                if not result.get("reviewed_code"):
                    result["reviewed_code"] = generated_code
            except json.JSONDecodeError:
                # Fallback: código sem alterações
                result = {
                    "reviewed_code": generated_code,
                    "corrections_made": [],
                    "quality_score": 75,
                    "recommendations": ["Revisar manualmente o código gerado"]
                }
            
            return result
            
        except Exception as e:
            raise RuntimeError(f"Erro no agente revisor: {str(e)}")