import os
import sys
from pathlib import Path
from typing import Optional, List, Dict, Any
import json

class FileHelper:
    """Utilitários para manipulação de arquivos"""
    
    @staticmethod
    def ensure_directory(path: Path) -> Path:
        """Garante que diretório existe"""
        path.mkdir(parents=True, exist_ok=True)
        return path
    
    @staticmethod
    def read_file(file_path: Path, encoding: str = 'utf-8') -> Optional[str]:
        """Lê arquivo texto"""
        try:
            with open(file_path, 'r', encoding=encoding) as f:
                return f.read()
        except Exception:
            return None
    
    @staticmethod
    def write_file(file_path: Path, content: str, encoding: str = 'utf-8') -> bool:
        """Escreve arquivo texto"""
        try:
            FileHelper.ensure_directory(file_path.parent)
            with open(file_path, 'w', encoding=encoding) as f:
                f.write(content)
            return True
        except Exception:
            return False
    
    @staticmethod
    def read_json(file_path: Path) -> Optional[Dict[str, Any]]:
        """Lê arquivo JSON"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                return json.load(f)
        except Exception:
            return None
    
    @staticmethod
    def write_json(file_path: Path, data: Dict[str, Any]) -> bool:
        """Escreve arquivo JSON"""
        try:
            FileHelper.ensure_directory(file_path.parent)
            with open(file_path, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=2, ensure_ascii=False)
            return True
        except Exception:
            return False

class StringHelper:
    """Utilitários para manipulação de strings"""
    
    @staticmethod
    def sanitize_filename(filename: str) -> str:
        """Sanitiza nome de arquivo"""
        # Remove caracteres inválidos
        invalid_chars = '<>:"/\\|?*'
        for char in invalid_chars:
            filename = filename.replace(char, '_')
        
        # Remove espaços duplos e limita tamanho
        filename = ' '.join(filename.split())
        filename = filename[:100]  # Limite de 100 caracteres
        
        return filename
    
    @staticmethod
    def truncate(text: str, max_length: int = 50, suffix: str = "...") -> str:
        """Trunca texto se necessário"""
        if len(text) <= max_length:
            return text
        return text[:max_length - len(suffix)] + suffix
    
    @staticmethod
    def format_code_block(code: str, language: str = "abap") -> str:
        """Formata bloco de código"""
        return f"```{language}\n{code}\n```"

class SystemHelper:
    """Utilitários do sistema"""
    
    @staticmethod
    def get_terminal_size() -> tuple[int, int]:
        """Obtém tamanho do terminal"""
        try:
            size = os.get_terminal_size()
            return size.columns, size.lines
        except OSError:
            return 80, 24  # Padrão
    
    @staticmethod
    def clear_screen():
        """Limpa tela do terminal"""
        os.system('cls' if os.name == 'nt' else 'clear')
    
    @staticmethod
    def get_user_home() -> Path:
        """Obtém diretório home do usuário"""
        return Path.home()
    
    @staticmethod
    def is_windows() -> bool:
        """Verifica se é Windows"""
        return os.name == 'nt'

class ABAPHelper:
    """Utilitários específicos para ABAP"""
    
    @staticmethod
    def format_abap_code(code: str) -> str:
        """Formata código ABAP básico"""
        lines = code.split('\n')
        formatted_lines = []
        indent_level = 0
        
        for line in lines:
            stripped = line.strip().upper()
            
            # Diminuir indentação antes de certas palavras-chave
            if any(stripped.startswith(keyword) for keyword in 
                   ['ENDIF', 'ENDLOOP', 'ENDFORM', 'ENDFUNCTION', 'ENDMETHOD', 'ENDCLASS']):
                indent_level = max(0, indent_level - 1)
            
            # Aplicar indentação
            if stripped:
                formatted_lines.append('  ' * indent_level + stripped)
            else:
                formatted_lines.append('')
            
            # Aumentar indentação após certas palavras-chave
            if any(stripped.startswith(keyword) for keyword in 
                   ['IF ', 'LOOP ', 'FORM ', 'FUNCTION ', 'METHOD ', 'CLASS ']):
                indent_level += 1
        
        return '\n'.join(formatted_lines)
    
    @staticmethod
    def extract_program_name(code: str) -> Optional[str]:
        """Extrai nome do programa do código ABAP"""
        for line in code.split('\n'):
            if line.strip().upper().startswith('REPORT '):
                parts = line.strip().split()
                if len(parts) > 1:
                    return parts[1].rstrip('.')
        return None
    
    @staticmethod
    def get_abap_keywords() -> List[str]:
        """Retorna lista de palavras-chave ABAP"""
        return [
            'DATA', 'TYPE', 'LIKE', 'VALUE', 'INITIAL', 'CONSTANTS',
            'TABLES', 'SELECT', 'FROM', 'WHERE', 'INTO', 'FORM',
            'ENDFORM', 'FUNCTION', 'ENDFUNCTION', 'CLASS', 'ENDCLASS',
            'METHOD', 'ENDMETHOD', 'IF', 'ENDIF', 'ELSE', 'ELSEIF',
            'LOOP', 'ENDLOOP', 'WHILE', 'ENDWHILE', 'DO', 'ENDDO',
            'CASE', 'ENDCASE', 'WHEN', 'OTHERS', 'REPORT', 'PROGRAM'
        ]