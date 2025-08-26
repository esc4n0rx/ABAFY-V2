import os
from openai import OpenAI
from typing import List, Optional
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
    
    # Adicionar estes métodos em abapfy/ai/client.py após o método debug_code

    def review_code(self, code_chunks: List[str], context: str = "") -> str:
        """Faz review de código usando chunks menores"""
        if not self.client:
            raise RuntimeError("Cliente AI não configurado")
        
        # Criar prompt contextualizado para chunks
        prompt = f"""
Você é um especialista em Code Review ABAP. Analise os seguintes chunks de código:

CONTEXTO: {context}

CHUNKS DE CÓDIGO:
{self._format_chunks_for_review(code_chunks)}

Forneça uma análise detalhada focando em:
1. 🚀 Performance e otimizações
2. 🔒 Segurança e validações  
3. 📋 Boas práticas ABAP
4. 🔧 Manutenibilidade
5. 💡 Sugestões de melhoria

Use formato claro com emojis e seja específico nas recomendações.
"""
        return self._make_request(prompt)
    
    def debug_code(self, code_chunks: List[str], error_context: str = "") -> str:
        """Faz debug de código com contexto específico"""
        if not self.client:
            raise RuntimeError("Cliente AI não configurado")
        
        prompt = f"""
Você é um especialista em Debug ABAP. Analise o código para identificar problemas:

CONTEXTO DO ERRO: {error_context}

CHUNKS DE CÓDIGO:
{self._format_chunks_for_debug(code_chunks)}

Identifique:
1. 🐛 Possíveis bugs e erros
2. ⚠️ Pontos de falha potenciais
3. 🔍 Variáveis não inicializadas
4. 🧠 Problemas de lógica
5. 🛠️ Sugestões de correção

Seja específico sobre localização e causa dos problemas.
"""
        return self._make_request(prompt)
    
    def _format_chunks_for_review(self, chunks: List[str]) -> str:
        """Formata chunks para análise de review"""
        formatted = []
        for i, chunk in enumerate(chunks, 1):
            formatted.append(f"=== CHUNK {i} ===\n{chunk}\n")
        return "\n".join(formatted)
    
    def _format_chunks_for_debug(self, chunks: List[str]) -> str:
        """Formata chunks para análise de debug"""
        formatted = []
        for i, chunk in enumerate(chunks, 1):
            formatted.append(f"=== CÓDIGO SUSPEITO {i} ===\n{chunk}\n")
        return "\n".join(formatted)
    
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