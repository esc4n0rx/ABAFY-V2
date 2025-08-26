import json
from typing import Dict, Any, Optional
from pathlib import Path
import click
from abapfy.agents.base_agent import BaseAgent
from abapfy.ui.colors import print_colored

class PromptRefinerAgent(BaseAgent):
    """Agente responsável por refinar prompts do usuário"""
    
    def __init__(self, ai_client, config):
        super().__init__(ai_client, config, "prompt_refiner")
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Executa refinamento do prompt"""
        user_prompt = input_data.get("user_prompt", "")
        prompt_file_path = input_data.get("prompt_file_path")
        generation_type = input_data.get("generation_type", "PROGRAM")
        
        # Ler arquivo de prompt se fornecido
        prompt_file_content = ""
        if prompt_file_path and Path(prompt_file_path).exists():
            try:
                with open(prompt_file_path, 'r', encoding='utf-8') as f:
                    prompt_file_content = f"Conteúdo do arquivo: {f.read()}"
                print_colored(f"✅ Arquivo de prompt carregado: {prompt_file_path}", "green")
            except Exception as e:
                print_colored(f"⚠️ Erro ao carregar arquivo: {str(e)}", "yellow")
        
        # Construir contexto para o prompt
        context = {
            "user_prompt": user_prompt,
            "prompt_file_content": prompt_file_content,
            "generation_type": generation_type
        }
        
        # Gerar prompt refinado
        refined_prompt = self._build_prompt(context)
        
        try:
            # Fazer requisição para IA
            response = self.ai_client._make_request(refined_prompt)
            
            # Tentar parsear JSON
            try:
                result = json.loads(response)
            except json.JSONDecodeError:
                # Se não for JSON válido, criar estrutura básica
                result = {
                    "refined_prompt": response,
                    "requirements": {"functional": [], "technical": {}},
                    "complexity_score": 5,
                    "suggested_templates": []
                }
            
            return result
            
        except Exception as e:
            raise RuntimeError(f"Erro no agente refinador: {str(e)}")
    
    def get_prompt_from_file(self) -> Optional[str]:
        """Permite usuário selecionar arquivo de prompt"""
        print_colored("\n📁 Você pode carregar um arquivo .txt com a descrição completa:", "cyan")
        
        if click.confirm("Deseja carregar um arquivo de prompt?"):
            file_path = click.prompt(
                "Caminho do arquivo .txt",
                type=str
            )
            
            file_path = Path(file_path)
            if file_path.exists() and file_path.suffix.lower() == '.txt':
                return str(file_path)
            else:
                print_colored("❌ Arquivo não encontrado ou não é .txt", "red")
        
        return None