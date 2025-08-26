import json
from typing import Dict, Any
from pathlib import Path
from abapfy.agents.base_agent import BaseAgent
from abapfy.templates.manager import TemplateManager

class CodeDeveloperAgent(BaseAgent):
    """Agente responsável pelo desenvolvimento do código ABAP"""
    
    def __init__(self, ai_client, config):
        super().__init__(ai_client, config, "code_developer")
        self.template_manager = TemplateManager()
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Executa desenvolvimento do código"""
        refined_prompt = input_data.get("refined_prompt", "")
        requirements = input_data.get("requirements", {})
        selected_template = input_data.get("selected_template")
        customizations_needed = input_data.get("customizations_needed", [])
        
        # Carregar conteúdo do template se selecionado
        template_content = ""
        if selected_template and selected_template.get("path"):
            template_content = self.template_manager.get_template_content(
                selected_template["path"]
            )
        
        # Construir contexto
        context = {
            "refined_prompt": refined_prompt,
            "requirements": json.dumps(requirements, indent=2),
            "selected_template": json.dumps(selected_template, indent=2) if selected_template else "None",
            "customizations_needed": json.dumps(customizations_needed, indent=2),
            "template_content": template_content
        }
        
        # Gerar código
        development_prompt = self._build_prompt(context)
        
        try:
            response = self.ai_client._make_request(development_prompt)
            
            try:
                result = json.loads(response)
            except json.JSONDecodeError:
                # Se não for JSON, assumir que é o código direto
                result = {
                    "generated_code": response,
                    "implementation_notes": ["Código gerado diretamente"],
                    "dependencies": [],
                    "next_steps": ["Ativar no sistema SAP"]
                }
            
            return result
            
        except Exception as e:
            raise RuntimeError(f"Erro no agente desenvolvedor: {str(e)}")