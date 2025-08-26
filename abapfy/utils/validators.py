import re
from typing import List, Dict, Any

class ABAPValidator:
    """Validador para código e dados ABAP"""
    
    @staticmethod
    def validate_program_name(name: str) -> tuple[bool, str]:
        """Valida nome de programa ABAP"""
        if not name:
            return False, "Nome não pode estar vazio"
        
        if len(name) > 30:
            return False, "Nome deve ter no máximo 30 caracteres"
        
        if not re.match(r'^[A-Z][A-Z0-9_]*$', name.upper()):
            return False, "Nome deve começar com letra e conter apenas letras, números e underscore"
        
        return True, "Nome válido"
    
    @staticmethod
    def validate_variable_name(name: str) -> tuple[bool, str]:
        """Valida nome de variável ABAP"""
        if not name:
            return False, "Nome da variável não pode estar vazio"
        
        if len(name) > 30:
            return False, "Nome da variável deve ter no máximo 30 caracteres"
        
        if not re.match(r'^[A-Za-z][A-Za-z0-9_]*$', name):
            return False, "Nome da variável deve começar com letra"
        
        reserved_words = [
            'DATA', 'TYPE', 'LIKE', 'VALUE', 'INITIAL', 'CONSTANTS',
            'TABLES', 'SELECT', 'FROM', 'WHERE', 'INTO', 'FORM',
            'ENDFORM', 'FUNCTION', 'ENDFUNCTION', 'CLASS', 'ENDCLASS'
        ]
        
        if name.upper() in reserved_words:
            return False, f"'{name}' é uma palavra reservada ABAP"
        
        return True, "Nome de variável válido"
    
    @staticmethod
    def validate_table_name(name: str) -> tuple[bool, str]:
        """Valida nome de tabela SAP"""
        if not name:
            return False, "Nome da tabela não pode estar vazio"
        
        if len(name) > 16:
            return False, "Nome da tabela deve ter no máximo 16 caracteres"
        
        if not re.match(r'^[A-Z][A-Z0-9_]*$', name.upper()):
            return False, "Nome da tabela deve começar com letra maiúscula"
        
        return True, "Nome de tabela válido"

class InputValidator:
    """Validador para entradas do usuário"""
    
    @staticmethod
    def validate_email(email: str) -> tuple[bool, str]:
        """Valida formato de email"""
        pattern = r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$'
        if re.match(pattern, email):
            return True, "Email válido"
        return False, "Formato de email inválido"
    
    @staticmethod
    def validate_non_empty(value: str, field_name: str = "Campo") -> tuple[bool, str]:
        """Valida se campo não está vazio"""
        if not value or not value.strip():
            return False, f"{field_name} não pode estar vazio"
        return True, "Campo válido"
    
    @staticmethod
    def validate_length(value: str, min_len: int = 0, max_len: int = 255, 
                       field_name: str = "Campo") -> tuple[bool, str]:
        """Valida comprimento do campo"""
        if len(value) < min_len:
            return False, f"{field_name} deve ter pelo menos {min_len} caracteres"
        
        if len(value) > max_len:
            return False, f"{field_name} deve ter no máximo {max_len} caracteres"
        
        return True, "Comprimento válido"