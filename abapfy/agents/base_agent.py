from abc import ABC, abstractmethod
from typing import Dict, Any, Optional
import json
from pathlib import Path
from abapfy.ai.client import AIClient
from abapfy.config.manager import ConfigManager

class BaseAgent(ABC):
    """Classe base para todos os agentes do sistema"""
    
    def __init__(self, ai_client: AIClient, config: ConfigManager, agent_name: str):
        self.ai_client = ai_client
        self.config = config
        self.agent_name = agent_name
        self.prompts_dir = Path(__file__).parent / "prompts"
        self._prompt_config = self._load_prompt_config()
    
    def _load_prompt_config(self) -> Dict[str, Any]:
        """Carrega configuração de prompts do agente"""
        prompt_file = self.prompts_dir / f"{self.agent_name.lower()}.json"
        try:
            with open(prompt_file, 'r', encoding='utf-8') as f:
                return json.load(f)
        except Exception as e:
            raise RuntimeError(f"Erro ao carregar prompts do agente {self.agent_name}: {str(e)}")
    
    def _build_prompt(self, context: Dict[str, Any]) -> str:
        """Constrói prompt baseado na configuração e contexto"""
        prompt_template = self._prompt_config.get("prompt_template", "")
        
        # Substituir variáveis do template
        for key, value in context.items():
            placeholder = f"{{{key}}}"
            if isinstance(value, str):
                prompt_template = prompt_template.replace(placeholder, value)
            elif isinstance(value, (list, dict)):
                prompt_template = prompt_template.replace(placeholder, json.dumps(value, indent=2))
        
        return prompt_template
    
    @abstractmethod
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Executa a lógica principal do agente"""
        pass
    
    def get_agent_info(self) -> Dict[str, Any]:
        """Retorna informações sobre o agente"""
        return {
            "name": self.agent_name,
            "description": self._prompt_config.get("description", ""),
            "input_schema": self._prompt_config.get("input_schema", {}),
            "output_schema": self._prompt_config.get("output_schema", {})
        }