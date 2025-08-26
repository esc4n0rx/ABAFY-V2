from abc import ABC, abstractmethod
from abapfy.ai.client import AIClient
from abapfy.config.manager import ConfigManager
from abapfy.ui.colors import print_colored
import click
from pathlib import Path
import re

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
        """Obtém descrição do usuário, com suporte para múltiplas linhas."""
        print_colored(f"\n{prompt_text}", self.config.get("text_color", "cyan"))
        print_colored("Você pode colar um texto com múltiplas linhas. Quando terminar, pressione Enter em uma linha vazia.", "yellow")
        
        lines = []
        while True:
            try:
                line = input()
                if line == "":
                    break
                lines.append(line)
            except EOFError:
                break
        
        return "\n".join(lines).strip()
    
    def _save_to_file(self, content: str, filename: str, extension: str = ".abap"):
        """Salva conteúdo em arquivo com validação de código ABAP"""
        try:
            if click.confirm(
                click.style("Deseja salvar o código em um arquivo?", 
                          fg=self.config.get("text_color", "cyan"))
            ):
                # Limpar e validar conteúdo antes de salvar
                clean_content = self._clean_abap_content(content)
                
                if not clean_content.strip():
                    print_colored("❌ Conteúdo vazio ou inválido. Arquivo não será salvo.", "red")
                    return
                
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
                    f.write(clean_content)
                
                print_colored(f"✅ Arquivo salvo em: {file_path}", "green")
                print_colored(f"📊 {len(clean_content.splitlines())} linhas de código ABAP", "cyan")
                
        except Exception as e:
            print_colored(f"❌ Erro ao salvar arquivo: {str(e)}", "red")
    
    def _clean_abap_content(self, content: str) -> str:
        """Limpa conteúdo garantindo que seja apenas código ABAP"""
        if not content.strip():
            return ""
        
        # Remover texto explicativo inicial
        clean_content = content
        
        # Remover JSON se presente
        clean_content = re.sub(r'```json.*?```', '', clean_content, flags=re.DOTALL)
        
        # Remover texto introdutório comum
        intro_patterns = [
            r'^.*?(?=\*&-{5,})',  # Até encontrar cabeçalho ABAP
            r'^.*?(?=REPORT\s+\w+)',
            r'^.*?(?=FUNCTION\s+\w+)',
            r'^.*?(?=CLASS\s+\w+\s+DEFINITION)'
        ]
        
        for pattern in intro_patterns:
            match = re.search(pattern, clean_content, re.DOTALL | re.IGNORECASE)
            if match:
                clean_content = clean_content[match.end():]
                break
        
        # Remover markdown
        clean_content = re.sub(r'```\w*\n?', '', clean_content)
        clean_content = re.sub(r'\n?```', '', clean_content)
        
        # Remover linhas de JSON no final
        lines = clean_content.split('\n')
        abap_lines = []
        
        for line in lines:
            # Parar se encontrar indicadores de JSON
            if any(indicator in line for indicator in ['"generated_code"', '"implementation_notes"', '{"', '},', '"}']):
                break
            abap_lines.append(line)
        
        result = '\n'.join(abap_lines).strip()
        
        # Validação final - deve ter pelo menos indicadores básicos de ABAP
        if self._is_valid_abap_content(result):
            return result
        else:
            print_colored("⚠️ Conteúdo pode não ser código ABAP válido", "yellow")
            return result
    
    def _is_valid_abap_content(self, content: str) -> bool:
        """Verifica se o conteúdo parece ser código ABAP válido"""
        if not content.strip():
            return False
        
        abap_indicators = [
            r'\*&-{5,}',
            r'REPORT\s+\w+',
            r'FUNCTION\s+\w+', 
            r'CLASS\s+\w+\s+DEFINITION',
            r'DATA\s*:',
            r'SELECT\s+.*FROM'
        ]
        
        matches = sum(1 for pattern in abap_indicators if re.search(pattern, content, re.IGNORECASE))
        return matches >= 1
    
    def _display_code(self, code: str, title: str):
        """Exibe código formatado"""
        print_colored(f"\n🎉 {title}", "green", bold=True)
        print_colored("=" * 60, "green")
        print_colored("```abap", "yellow")
        print(code)
        print_colored("```", "yellow")
        print_colored("=" * 60, "green")