from abc import ABC, abstractmethod
from abapfy.ai.client import AIClient
from abapfy.config.manager import ConfigManager
from abapfy.ui.colors import print_colored
import click
from pathlib import Path

class BaseGenerator(ABC):
    """Classe base para geradores de código ABAP"""
    
    def __init__(self, ai_client: AIClient, config: ConfigManager):
        self.ai_client = ai_client
        self.config = config
    
    @abstractmethod
    def generate(self):
        """Método abstrato para geração de código"""
        pass
    
    def _get_description(self, prompt_text: str) -> str:
        """Obtém descrição do usuário"""
        description = click.prompt(
            click.style(prompt_text, fg=self.config.get("text_color", "cyan")),
            type=str
        )
        return description.strip()
    
    def _save_to_file(self, content: str, filename: str, extension: str = ".abap"):
        """Salva conteúdo em arquivo"""
        try:
            if click.confirm(
                click.style("Deseja salvar o código em um arquivo?", 
                          fg=self.config.get("text_color", "cyan"))
            ):
                default_filename = f"{filename}{extension}"
                save_filename = click.prompt(
                    click.style("Nome do arquivo", 
                              fg=self.config.get("text_color", "cyan")),
                    default=default_filename
                )
                
                # Criar diretório se não existir
                output_dir = Path("output")
                output_dir.mkdir(exist_ok=True)
                
                file_path = output_dir / save_filename
                
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                
                print_colored(f"✅ Arquivo salvo em: {file_path}", "green")
                
        except Exception as e:
            print_colored(f"❌ Erro ao salvar arquivo: {str(e)}", "red")
    
    def _display_code(self, code: str, title: str):
        """Exibe código formatado"""
        print_colored(f"\n🎉 {title}", "green", bold=True)
        print_colored("=" * 60, "green")
        print_colored("```abap", "yellow")
        print(code)
        print_colored("```", "yellow")
        print_colored("=" * 60, "green")