# abapfy/utils/file_manager.py
import os
from pathlib import Path
from typing import List, Dict, Optional, Tuple
from abapfy.ui.colors import print_colored

class ABAPFileManager:
    """Gerenciador para arquivos ABAP na pasta de códigos"""
    
    def __init__(self, base_dir: Optional[str] = None):
        if base_dir:
            self.codes_dir = Path(base_dir)
        else:
            self.codes_dir = Path.cwd() / "abap_codes"
        
        # Criar diretório se não existir
        self.codes_dir.mkdir(exist_ok=True)
        
        # Extensões ABAP suportadas
        self.abap_extensions = {'.abap', '.txt', '.inc', '.prog'}
    
    def list_abap_files(self) -> List[Dict[str, str]]:
        """Lista todos os arquivos ABAP na pasta"""
        files = []
        
        try:
            for file_path in self.codes_dir.iterdir():
                if file_path.is_file() and file_path.suffix.lower() in self.abap_extensions:
                    # Obter informações do arquivo
                    stat = file_path.stat()
                    size_kb = round(stat.st_size / 1024, 2)
                    
                    files.append({
                        'name': file_path.name,
                        'path': str(file_path),
                        'size_kb': size_kb,
                        'modified': self._format_timestamp(stat.st_mtime),
                        'lines': self._count_lines(file_path)
                    })
            
            # Ordenar por data de modificação (mais recente primeiro)
            files.sort(key=lambda x: x['modified'], reverse=True)
            
        except Exception as e:
            print_colored(f"❌ Erro ao listar arquivos: {str(e)}", "red")
        
        return files
    
    def get_file_content(self, file_path: str) -> Optional[str]:
        """Obtém conteúdo de arquivo ABAP"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                return f.read()
        except UnicodeDecodeError:
            # Tentar com encoding alternativo
            try:
                with open(file_path, 'r', encoding='latin-1') as f:
                    return f.read()
            except Exception:
                return None
        except Exception:
            return None
    
    def validate_abap_file(self, file_path: str) -> Tuple[bool, str]:
        """Valida se arquivo é ABAP válido"""
        if not os.path.exists(file_path):
            return False, "Arquivo não encontrado"
        
        if not Path(file_path).suffix.lower() in self.abap_extensions:
            return False, f"Extensão não suportada. Use: {', '.join(self.abap_extensions)}"
        
        content = self.get_file_content(file_path)
        if not content:
            return False, "Não foi possível ler o arquivo"
        
        if not content.strip():
            return False, "Arquivo está vazio"
        
        # Verificações básicas de código ABAP
        content_upper = content.upper()
        abap_keywords = ['REPORT', 'PROGRAM', 'CLASS', 'FUNCTION', 'METHOD', 'DATA', 'SELECT']
        
        has_abap_content = any(keyword in content_upper for keyword in abap_keywords)
        if not has_abap_content:
            return False, "Arquivo não parece conter código ABAP válido"
        
        return True, "Arquivo ABAP válido"
    
    def create_codes_directory_structure(self):
        """Cria estrutura de diretórios para organizar códigos"""
        subdirs = ['reports', 'classes', 'functions', 'includes', 'debug', 'review']
        
        for subdir in subdirs:
            (self.codes_dir / subdir).mkdir(exist_ok=True)
        
        # Criar arquivo README
        readme_content = """# Pasta de Códigos ABAP

Esta pasta é usada pelo ABAPFY para análise de código.

## Estrutura:
- reports/   - Programas tipo REPORT
- classes/   - Classes ABAP  
- functions/ - Módulos de função
- includes/  - Includes
- debug/     - Arquivos para debug
- review/    - Arquivos para code review

## Uso:
1. Coloque seus arquivos .abap nas pastas apropriadas
2. Use o ABAPFY para análise de Code Review ou Debug
3. Os relatórios serão salvos na pasta de saída

## Extensões suportadas:
.abap, .txt, .inc, .prog
"""
        
        readme_path = self.codes_dir / "README.md"
        if not readme_path.exists():
            with open(readme_path, 'w', encoding='utf-8') as f:
                f.write(readme_content)
    
    def _count_lines(self, file_path: Path) -> int:
        """Conta linhas do arquivo"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                return sum(1 for _ in f)
        except:
            return 0
    
    def _format_timestamp(self, timestamp: float) -> str:
        """Formata timestamp para exibição"""
        from datetime import datetime
        return datetime.fromtimestamp(timestamp).strftime("%d/%m/%Y %H:%M")
    
    def display_files_menu(self, files: List[Dict[str, str]]) -> Optional[str]:
        """Exibe menu de seleção de arquivos"""
        if not files:
            print_colored("📁 Nenhum arquivo ABAP encontrado na pasta 'abap_codes'", "yellow")
            print_colored("💡 Coloque seus arquivos .abap na pasta para análise", "cyan")
            return None
        
        print_colored(f"\n📁 Arquivos ABAP encontrados ({len(files)}):", "cyan", bold=True)
        print_colored("=" * 60, "cyan")
        
        for i, file_info in enumerate(files, 1):
            print_colored(f"{i:2d}. ", "white", end="")
            print_colored(f"{file_info['name']}", "green", bold=True)
            print_colored(f"    📊 {file_info['lines']} linhas | {file_info['size_kb']} KB | {file_info['modified']}", "yellow")
        
        print_colored("\n0. ↩️  Voltar ao menu principal", "yellow")
        
        try:
            choice = input(print_colored("\nEscolha o arquivo: ", "cyan", end=""))
            choice_num = int(choice)
            
            if choice_num == 0:
                return None
            elif 1 <= choice_num <= len(files):
                return files[choice_num - 1]['path']
            else:
                print_colored("❌ Opção inválida!", "red")
                return None
                
        except ValueError:
            print_colored("❌ Digite um número válido!", "red")
            return None
        except KeyboardInterrupt:
            return None