import click
from abapfy.generators.base import BaseGenerator
from abapfy.ui.colors import print_colored

class ModuleGenerator(BaseGenerator):
    """Gerador de módulos ABAP"""
    
    MODULE_TYPES = {
        "1": ("FUNCTION", "Módulo de função"),
        "2": ("METHOD", "Método de classe"),
        "3": ("SUBROUTINE", "Sub-rotina (FORM)"),
        "4": ("USER-EXIT", "User Exit"),
        "5": ("BADI", "Business Add-In"),
        "6": ("ENHANCEMENT", "Enhancement Point")
    }
    
    def generate(self):
        """Gera um módulo ABAP"""
        try:
            # Selecionar tipo de módulo
            module_type = self._select_module_type()
            if not module_type:
                return
            
            # Obter descrição
            description = self._get_description(
                f"Descreva o {module_type.lower()} que você deseja criar"
            )
            
            if not description:
                print_colored("❌ Descrição não pode estar vazia.", "red")
                return
            
            # Obter parâmetros
            parameters = self._get_parameters(module_type)
            
            # Construir descrição completa
            full_description = self._build_module_description(
                description, module_type, parameters
            )
            
            print_colored(f"🤖 Gerando {module_type.lower()}...", "yellow")
            
            # Gerar código
            code = self.ai_client.generate_module(full_description, module_type)
            
            # Exibir resultado
            self._display_code(
                code,
                f"{module_type.title()} gerado com sucesso!"
            )
            
            # Salvar arquivo
            filename = f"module_{module_type.lower().replace('-', '_')}"
            self._save_to_file(code, filename)
            
        except Exception as e:
            print_colored(f"❌ Erro ao gerar módulo: {str(e)}", "red")
    
    def _select_module_type(self) -> str:
        """Seleciona tipo de módulo"""
        print_colored("\n📦 Selecione o tipo de módulo:", "cyan")
        
        for key, (mod_type, description) in self.MODULE_TYPES.items():
            print_colored(f"  {key}. {mod_type} - {description}", "white")
        
        choice = click.prompt(
            click.style("\nEscolha o tipo (1-6)", 
                       fg=self.config.get("text_color", "cyan")),
            type=click.Choice(list(self.MODULE_TYPES.keys()))
        )
        
        return self.MODULE_TYPES[choice][0]
    
    def _get_parameters(self, module_type: str) -> dict:
        """Obtém parâmetros específicos do módulo"""
        parameters = {}
        
        if module_type in ["FUNCTION", "METHOD"]:
            # Parâmetros de importação
            import_params = click.prompt(
                "Parâmetros de importação (formato: PARAM1 TYPE TYPE1, PARAM2 TYPE TYPE2)",
                default="",
                show_default=False
            )
            
            # Parâmetros de exportação
            export_params = click.prompt(
                "Parâmetros de exportação (formato: PARAM1 TYPE TYPE1, PARAM2 TYPE TYPE2)",
                default="",
                show_default=False
            )
            
            # Parâmetros de mudança
            changing_params = click.prompt(
                "Parâmetros de mudança (formato: PARAM1 TYPE TYPE1, PARAM2 TYPE TYPE2)",
                default="",
                show_default=False
            )
            
            parameters.update({
                "importing": import_params,
                "exporting": export_params,
                "changing": changing_params
            })
            
            if module_type == "FUNCTION":
                # Exceções
                exceptions = click.prompt(
                    "Exceções (separadas por vírgula)",
                    default="",
                    show_default=False
                )
                parameters["exceptions"] = exceptions
        
        elif module_type == "SUBROUTINE":
            # Parâmetros da FORM
            form_params = click.prompt(
                "Parâmetros da FORM (USING/CHANGING)",
                default="",
                show_default=False
            )
            parameters["form_parameters"] = form_params
        
        elif module_type in ["USER-EXIT", "BADI"]:
            # Nome do enhancement
            enhancement_name = click.prompt(
                f"Nome do {module_type.lower()}",
                type=str
            )
            parameters["enhancement_name"] = enhancement_name
        
        return parameters
    
    def _build_module_description(self, description: str, module_type: str, 
                                parameters: dict) -> str:
        """Constrói descrição completa do módulo"""
        full_desc = f"Tipo: {module_type}\nDescrição: {description}\n"
        
        if parameters:
            full_desc += "\nParâmetros e configurações:\n"
            for key, value in parameters.items():
                if value:  # Apenas incluir valores não vazios
                    full_desc += f"- {key}: {value}\n"
        
        return full_desc