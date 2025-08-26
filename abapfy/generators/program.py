import click
from abapfy.generators.base import BaseGenerator
from abapfy.ui.colors import print_colored

class ProgramGenerator(BaseGenerator):
    """Gerador de programas ABAP"""
    
    PROGRAM_TYPES = {
        "1": ("REPORT", "Programa de relatório"),
        "2": ("MODULE-POOL", "Module Pool (telas)"),
        "3": ("FUNCTION-GROUP", "Grupo de funções"),
        "4": ("CLASS", "Classe ABAP"),
        "5": ("INTERFACE", "Interface ABAP")
    }
    
    def generate(self):
        """Gera um programa ABAP"""
        try:
            # Selecionar tipo de programa
            program_type = self._select_program_type()
            if not program_type:
                return
            
            # Obter descrição
            description = self._get_description(
                "Descreva o programa que você deseja criar"
            )
            
            if not description:
                print_colored("❌ Descrição não pode estar vazia.", "red")
                return
            
            # Obter informações adicionais
            additional_info = self._get_additional_info(program_type)
            
            # Construir descrição completa
            full_description = self._build_full_description(
                description, program_type, additional_info
            )
            
            print_colored("🤖 Gerando programa ABAP...", "yellow")
            
            # Gerar código
            code = self.ai_client.generate_program(full_description, program_type)
            
            # Exibir resultado
            self._display_code(
                code, 
                f"Programa {program_type.upper()} gerado com sucesso!"
            )
            
            # Salvar arquivo
            filename = f"program_{program_type.lower().replace('-', '_')}"
            self._save_to_file(code, filename)
            
        except Exception as e:
            print_colored(f"❌ Erro ao gerar programa: {str(e)}", "red")
    
    def _select_program_type(self) -> str:
        """Seleciona tipo de programa"""
        print_colored("\n📋 Selecione o tipo de programa:", "cyan")
        
        for key, (prog_type, description) in self.PROGRAM_TYPES.items():
            print_colored(f"  {key}. {prog_type} - {description}", "white")
        
        choice = click.prompt(
            click.style("\nEscolha o tipo (1-5)", 
                       fg=self.config.get("text_color", "cyan")),
            type=click.Choice(list(self.PROGRAM_TYPES.keys()))
        )
        
        return self.PROGRAM_TYPES[choice][0]
    
    def _get_additional_info(self, program_type: str) -> dict:
        """Obtém informações adicionais baseadas no tipo"""
        additional_info = {}
        
        if program_type == "REPORT":
            additional_info["has_selection_screen"] = click.confirm(
                "Incluir tela de seleção?"
            )
            additional_info["output_format"] = click.prompt(
                "Formato de saída (LIST/ALV)",
                default="ALV",
                type=click.Choice(["LIST", "ALV"])
            )
        
        elif program_type == "MODULE-POOL":
            additional_info["screen_count"] = click.prompt(
                "Número de telas",
                default=1,
                type=int
            )
            additional_info["has_table_control"] = click.confirm(
                "Incluir Table Control?"
            )
        
        elif program_type == "CLASS":
            additional_info["has_constructor"] = click.confirm(
                "Incluir construtor?"
            )
            additional_info["implements_interface"] = click.prompt(
                "Interface a implementar (deixe vazio se nenhuma)",
                default="",
                show_default=False
            )
        
        return additional_info
    
    def _build_full_description(self, description: str, program_type: str, 
                              additional_info: dict) -> str:
        """Constrói descrição completa para o prompt"""
        full_desc = f"Tipo: {program_type}\nDescrição: {description}\n"
        
        if additional_info:
            full_desc += "\nRequirements adicionais:\n"
            for key, value in additional_info.items():
                if value:  # Apenas incluir valores não vazios/False
                    full_desc += f"- {key}: {value}\n"
        
        return full_desc