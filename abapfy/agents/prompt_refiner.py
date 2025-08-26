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
        """Permite usuário selecionar arquivo de prompt com melhor UX"""
        print_colored("\n📁 Você pode carregar um arquivo .txt com a descrição completa:", "cyan")
        
        # Verificar se há arquivos de exemplo disponíveis
        examples_dir = Path("abapfy/exemplos")
        available_examples = []
        
        if examples_dir.exists():
            available_examples = list(examples_dir.glob("*.txt"))
            if available_examples:
                print_colored("\n💡 Arquivos de exemplo disponíveis:", "yellow")
                for i, example in enumerate(available_examples, 1):
                    print_colored(f"   {i}. {example.name}", "white")
                print_colored(f"   Para usar: {examples_dir.name}/{available_examples[0].name}", "cyan")
        
        if click.confirm("Deseja carregar um arquivo de prompt?"):
            while True:
                file_path = click.prompt(
                    "Caminho do arquivo .txt",
                    type=str
                )
                
                # Tentar diferentes possibilidades de caminho
                possible_paths = [
                    Path(file_path),  # Caminho como digitado
                    Path.cwd() / file_path,  # Relativo ao diretório atual
                    Path("abapfy") / file_path,  # Relativo à pasta abapfy
                ]
                
                # Se não tem extensão, adicionar .txt
                if not Path(file_path).suffix:
                    file_path += ".txt"
                    possible_paths.extend([
                        Path(file_path),
                        Path.cwd() / file_path,
                        Path("abapfy") / file_path,
                    ])
                
                found_file = None
                for path in possible_paths:
                    if path.exists() and path.suffix.lower() == '.txt':
                        found_file = path
                        break
                
                if found_file:
                    print_colored(f"✅ Arquivo encontrado: {found_file}", "green")
                    return str(found_file)
                else:
                    print_colored("❌ Arquivo não encontrado ou não é .txt", "red")
                    
                    # Sugerir arquivos próximos
                    if available_examples:
                        print_colored("\n💡 Sugestões:", "yellow")
                        for example in available_examples:
                            relative_path = f"abapfy/{examples_dir.name}/{example.name}"
                            print_colored(f"   {relative_path}", "cyan")
                    
                    if not click.confirm("Tentar outro arquivo?"):
                        break
        
        return None