import click
from abapfy.generators.base import BaseGenerator
from abapfy.ui.colors import print_colored
from abapfy.agents.agent_orchestrator import AgentOrchestrator
from abapfy.agents.prompt_refiner import PromptRefinerAgent

class ModuleGenerator(BaseGenerator):
    """Gerador de módulos ABAP usando sistema de agentes"""
    
    MODULE_TYPES = {
        "1": ("FUNCTION", "Módulo de função"),
        "2": ("METHOD", "Método de classe"),
        "3": ("SUBROUTINE", "Sub-rotina (FORM)"),
        "4": ("USER-EXIT", "User Exit"),
        "5": ("BADI", "Business Add-In"),
        "6": ("ENHANCEMENT", "Enhancement Point")
    }
    
    def __init__(self, ai_client, config):
        super().__init__(ai_client, config)
        self.orchestrator = AgentOrchestrator(ai_client, config)
    
    def generate(self):
        """Gera um módulo ABAP usando sistema de agentes"""
        try:
            # Selecionar tipo de módulo
            module_type = self._select_module_type()
            if not module_type:
                return
            
            # Opção de carregar arquivo de prompt
            prompt_refiner = PromptRefinerAgent(self.ai_client, self.config)
            prompt_file_path = prompt_refiner.get_prompt_from_file()
            
            # Obter descrição
            if prompt_file_path:
                print_colored("✅ Arquivo carregado. Você pode adicionar informações complementares:", "green")
                description = input("Informações adicionais (opcional): ").strip()
            else:
                description = self._get_description(
                    f"Descreva detalhadamente o {module_type.lower()} que você deseja criar"
                )
            
            if not description and not prompt_file_path:
                print_colored("❌ É necessário fornecer uma descrição ou arquivo de prompt.", "red")
                return
            
            # Obter parâmetros específicos (se necessário)
            additional_info = self._get_module_parameters(module_type)
            
            # Combinar descrição com parâmetros
            full_description = description
            if additional_info:
                full_description += f"\n\nInformações técnicas adicionais:\n{additional_info}"
            
            # Executar pipeline de agentes
            results = self.orchestrator.execute_pipeline(
                user_prompt=full_description,
                generation_type="MODULE", 
                prompt_file_path=prompt_file_path
            )
            
            # Obter código final
            final_code = results["reviewer"]["reviewed_code"]
            quality_score = results["reviewer"]["quality_score"]
            
            # Exibir código gerado
            print_colored("\n" + "="*70, "green")
            print_colored(f"📦 {module_type} ABAP GERADO", "green", bold=True)
            print_colored("="*70, "green")
            print_colored("```abap", "yellow")
            print(final_code)
            print_colored("```", "yellow")
            
            # Informações adicionais
            print_colored(f"\n⭐ Score de Qualidade: {quality_score}/100", "cyan")
            
            # Salvar arquivo
            filename = f"module_{module_type.lower().replace('-', '_')}"
            self._save_to_file(final_code, filename)
            
            # Mostrar próximos passos
            next_steps = results["developer"].get("next_steps", [])
            if next_steps:
                print_colored("\n📋 PRÓXIMOS PASSOS RECOMENDADOS:", "magenta", bold=True)
                for i, step in enumerate(next_steps, 1):
                    print_colored(f"   {i}. {step}", "white")
            
        except Exception as e:
            print_colored(f"❌ Erro ao gerar módulo: {str(e)}", "red")
    
    def _select_module_type(self) -> str:
        """Seleciona tipo de módulo"""
        print_colored("\n📦 Selecione o tipo de módulo:", "cyan")
        
        for key, (mod_type, description) in self.MODULE_TYPES.items():
            print_colored(f"  {key}. {mod_type} - {description}", "white")
        
        try:
            choice = click.prompt(
                click.style("\nEscolha o tipo (1-6)", 
                           fg=self.config.get("text_color", "cyan")),
                type=click.Choice(list(self.MODULE_TYPES.keys()))
            )
            return self.MODULE_TYPES[choice][0]
        except (click.Abort, KeyboardInterrupt):
            return None
    
    def _get_module_parameters(self, module_type: str) -> str:
        """Obtém parâmetros específicos do módulo de forma opcional"""
        text_color = self.config.get("text_color", "cyan")
        additional_info = []
        
        print_colored(f"\n⚙️ Parâmetros técnicos opcionais para {module_type}:", "cyan")
        print_colored("(Pressione Enter para pular qualquer campo)", "yellow")
        
        if module_type in ["FUNCTION", "METHOD"]:
            import_params = input("Parâmetros IMPORTING (ex: IV_PARAM TYPE STRING): ").strip()
            export_params = input("Parâmetros EXPORTING (ex: EV_RESULT TYPE STRING): ").strip()
            changing_params = input("Parâmetros CHANGING (ex: CV_DATA TYPE TABLE): ").strip()
            
            if import_params:
                additional_info.append(f"Parâmetros IMPORTING: {import_params}")
            if export_params:
                additional_info.append(f"Parâmetros EXPORTING: {export_params}")
            if changing_params:
                additional_info.append(f"Parâmetros CHANGING: {changing_params}")
            
            if module_type == "FUNCTION":
                exceptions = input("Exceções (separadas por vírgula): ").strip()
                if exceptions:
                    additional_info.append(f"Exceções: {exceptions}")
        
        elif module_type in ["USER-EXIT", "BADI"]:
            enhancement_name = input(f"Nome do {module_type.lower()}: ").strip()
            if enhancement_name:
                additional_info.append(f"Nome: {enhancement_name}")
        
        return "\n".join(additional_info)