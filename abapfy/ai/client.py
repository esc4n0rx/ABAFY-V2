import os
from openai import OpenAI
from typing import Optional
from abapfy.config.manager import ConfigManager
from abapfy.ai.prompts import ABAPPrompts
from dotenv import load_dotenv

load_dotenv()

class AIClient:
    """Cliente para interação com IA"""
    
    def __init__(self, config: ConfigManager):
        self.config = config
        self.client = self._init_client()
        self.prompts = ABAPPrompts()
    
    def _init_client(self) -> Optional[OpenAI]:
        """Inicializa cliente OpenAI"""
        api_key = os.getenv('OPENAI_API_KEY') or os.getenv('ARCEE_TOKEN')
        base_url = os.getenv('OPENAI_BASE_URL', 'https://conductor.arcee.ai/v1')
        
        if not api_key:
            return None
        
        return OpenAI(
            api_key=api_key,
            base_url=base_url
        )
    
    def is_configured(self) -> bool:
        """Verifica se cliente está configurado"""
        return self.client is not None
    
    def generate_program(self, description: str, program_type: str = "REPORT") -> str:
        """Gera um programa ABAP"""
        if not self.client:
            raise RuntimeError("Cliente AI não configurado")
        
        prompt = self.prompts.get_program_prompt(description, program_type)
        return self._make_request(prompt)
    
    def generate_module(self, description: str, module_type: str) -> str:
        """Gera um módulo ABAP"""
        if not self.client:
            raise RuntimeError("Cliente AI não configurado")
        
        prompt = self.prompts.get_module_prompt(description, module_type)
        return self._make_request(prompt)
    
    def debug_code(self, code: str) -> str:
        """Faz debug de código ABAP"""
        if not self.client:
            raise RuntimeError("Cliente AI não configurado")
        
        prompt = self.prompts.get_debug_prompt(code)
        return self._make_request(prompt)
    
    def review_code(self, code: str) -> str:
        """Faz review de código ABAP"""
        if not self.client:
            raise RuntimeError("Cliente AI não configurado")
        
        prompt = self.prompts.get_review_prompt(code)
        return self._make_request(prompt)
    
    def _make_request(self, prompt: str) -> str:
        """Faz requisição para a IA"""
        try:
            response = self.client.chat.completions.create(
                model=self.config.get("model", "auto"),
                messages=[{"role": "user", "content": prompt}],
                temperature=self.config.get("temperature", 0.4)
            )
            return response.choices[0].message.content
        except Exception as e:
            raise RuntimeError(f"Erro na requisição para IA: {str(e)}")