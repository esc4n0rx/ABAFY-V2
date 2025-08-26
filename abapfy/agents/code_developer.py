import json
from typing import Dict, Any
from pathlib import Path
from abapfy.agents.base_agent import BaseAgent
from abapfy.templates.manager import TemplateManager
import re

class CodeDeveloperAgent(BaseAgent):
    """Agente responsável pelo desenvolvimento do código ABAP"""
    
    def __init__(self, ai_client, config):
        super().__init__(ai_client, config, "code_developer")
        self.template_manager = TemplateManager()
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Executa desenvolvimento do código"""
        refined_prompt = input_data.get("refined_prompt", "")
        requirements = input_data.get("requirements", {})
        selected_template = input_data.get("selected_template")
        customizations_needed = input_data.get("customizations_needed", [])
        
        # Carregar conteúdo do template se selecionado
        template_content = ""
        if selected_template and selected_template.get("path"):
            template_content = self.template_manager.get_template_content(
                selected_template["path"]
            )
        
        # Construir contexto
        context = {
            "refined_prompt": refined_prompt,
            "requirements": json.dumps(requirements, indent=2),
            "selected_template": json.dumps(selected_template, indent=2) if selected_template else "None",
            "customizations_needed": json.dumps(customizations_needed, indent=2),
            "template_content": template_content
        }
        
        # Gerar código
        development_prompt = self._build_prompt(context)
        
        try:
            response = self.ai_client._make_request(development_prompt)
            
            # Tentar extrair código ABAP puro primeiro
            abap_code = self._extract_abap_code(response)
            
            if abap_code:
                # Código ABAP puro foi encontrado
                result = {
                    "generated_code": abap_code,
                    "implementation_notes": ["Código ABAP gerado com sucesso"],
                    "dependencies": [],
                    "next_steps": ["Ativar no sistema SAP", "Testar funcionalidade"]
                }
            else:
                # Tentar parsear como JSON
                try:
                    result = json.loads(response)
                    # Validar se tem o campo generated_code
                    if not result.get("generated_code"):
                        result["generated_code"] = self._fallback_code_extraction(response)
                except json.JSONDecodeError:
                    # Fallback: assumir que toda resposta é código
                    result = {
                        "generated_code": self._clean_response(response),
                        "implementation_notes": ["Código extraído diretamente da resposta"],
                        "dependencies": [],
                        "next_steps": ["Ativar no sistema SAP"]
                    }
            
            return result
            
        except Exception as e:
            raise RuntimeError(f"Erro no agente desenvolvedor: {str(e)}")
    
    def _extract_abap_code(self, response: str) -> str:
        """Extrai código ABAP puro da resposta"""
        # Remover blocos de código markdown
        abap_patterns = [
            r'```abap\n(.*?)\n```',
            r'```\n(.*?)\n```',
            r'```(.*?)```'
        ]
        
        for pattern in abap_patterns:
            match = re.search(pattern, response, re.DOTALL)
            if match:
                code = match.group(1).strip()
                if self._is_valid_abap_code(code):
                    return code
        
        # Se não encontrou em blocos markdown, verifica se a resposta inteira é código ABAP
        clean_response = self._clean_response(response)
        if self._is_valid_abap_code(clean_response):
            return clean_response
        
        return ""
    
    def _is_valid_abap_code(self, text: str) -> bool:
        """Verifica se o texto parece ser código ABAP válido"""
        if not text.strip():
            return False
        
        # Indicadores de código ABAP
        abap_indicators = [
            r'\*&-{5,}',  # Cabeçalho de programa
            r'REPORT\s+\w+',
            r'FUNCTION\s+\w+',
            r'CLASS\s+\w+\s+DEFINITION',
            r'METHOD\s+\w+',
            r'DATA\s*:',
            r'SELECT\s+.*FROM',
            r'START-OF-SELECTION',
            r'END-OF-SELECTION'
        ]
        
        # Deve ter pelo menos 2 indicadores ABAP
        matches = sum(1 for pattern in abap_indicators if re.search(pattern, text, re.IGNORECASE))
        
        # Não deve ter indicadores de JSON/texto explicativo
        non_abap_indicators = [
            r'^\s*{',  # Começa com JSON
            r'"generated_code"',
            r'"implementation_notes"',
            r'Vou desenvolver',
            r'O código a seguir'
        ]
        
        has_non_abap = any(re.search(pattern, text, re.IGNORECASE | re.MULTILINE) for pattern in non_abap_indicators)
        
        return matches >= 2 and not has_non_abap
    
    def _fallback_code_extraction(self, response: str) -> str:
        """Extração de código como fallback"""
        # Procurar por padrões específicos de código ABAP na resposta
        lines = response.split('\n')
        code_lines = []
        in_code_block = False
        
        for line in lines:
            # Detectar início de código ABAP
            if any(pattern in line.upper() for pattern in ['*&-----', 'REPORT ', 'FUNCTION ', 'CLASS ']):
                in_code_block = True
                code_lines.append(line)
            elif in_code_block:
                # Parar se encontrar indicadores de fim de código
                if any(pattern in line for pattern in ['"implementation_notes"', '"dependencies"', '"next_steps"']):
                    break
                code_lines.append(line)
        
        return '\n'.join(code_lines).strip() if code_lines else response
    
    def _clean_response(self, response: str) -> str:
        """Limpa a resposta removendo texto não-ABAP"""
        # Remover texto explicativo comum
        clean_response = response
        
        # Remover frases introdutórias
        intro_patterns = [
            r'^.*?Vou desenvolver.*?\n',
            r'^.*?O código a seguir.*?\n',
            r'^.*?Segue o código.*?\n',
        ]
        
        for pattern in intro_patterns:
            clean_response = re.sub(pattern, '', clean_response, flags=re.IGNORECASE | re.MULTILINE)
        
        # Remover blocos JSON se presentes
        clean_response = re.sub(r'```json.*?```', '', clean_response, flags=re.DOTALL)
        
        return clean_response.strip()