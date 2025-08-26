import json
from typing import Dict, Any, List
from pathlib import Path
from abapfy.agents.base_agent import BaseAgent
from abapfy.templates.manager import TemplateManager

class TemplateSelectorAgent(BaseAgent):
    """Agente responsável por selecionar templates adequados"""
    
    def __init__(self, ai_client, config):
        super().__init__(ai_client, config, "template_selector")
        self.template_manager = TemplateManager()
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Executa seleção de template"""
        refined_prompt = input_data.get("refined_prompt", "")
        requirements = input_data.get("requirements", {})
        generation_type = input_data.get("generation_type", "PROGRAM")
        suggested_templates = input_data.get("suggested_templates", [])
        
        # Obter templates disponíveis
        available_templates = self.template_manager.get_available_templates(generation_type)
        template_catalog = self.template_manager.get_template_catalog()
        
        # Construir contexto
        context = {
            "refined_prompt": refined_prompt,
            "requirements": json.dumps(requirements, indent=2),
            "generation_type": generation_type,
            "suggested_templates": json.dumps(suggested_templates, indent=2),
            "available_templates": json.dumps(available_templates, indent=2),
            "template_catalog": json.dumps(template_catalog, indent=2)
        }
        
        # Gerar análise de templates
        analysis_prompt = self._build_prompt(context)
        
        try:
            response = self.ai_client._make_request(analysis_prompt)
            
            try:
                result = json.loads(response)
            except json.JSONDecodeError:
                # Fallback: sem template selecionado
                result = {
                    "selected_template": None,
                    "template_analysis": {"customization_effort": "HIGH"},
                    "customizations_needed": [],
                    "fallback_strategy": "FROM_SCRATCH"
                }
            
            return result
            
        except Exception as e:
            raise RuntimeError(f"Erro no agente seletor: {str(e)}")