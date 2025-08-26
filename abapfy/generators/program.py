import click
from abapfy.generators.base import BaseGenerator
from abapfy.ui.colors import print_colored
from abapfy.agents.agent_orchestrator import AgentOrchestrator
from abapfy.agents.prompt_refiner import PromptRefinerAgent

class ProgramGenerator(BaseGenerator):
    """Gerador de programas ABAP usando sistema de agentes"""
    
    PROGRAM_TYPES = {
        "1": ("REPORT", "Programa de relatório"),
        "2": ("MODULE-POOL", "Module Pool (telas)"),
        "3": ("FUNCTION-GROUP", "Grupo de funções"),
        "4": ("CLASS", "Classe ABAP"),
        "5": ("INTERFACE", "Interface ABAP")
    }
    
    def __init__(self, ai_client, config):
        super().__init__(ai_client, config)
        self.orchestrator = AgentOrchestrator(ai_client, config)
    
    def generate(self):
        """Gera um programa ABAP usando sistema de agentes"""
        try:
            # Selecionar tipo de programa
            program_type = self._select_program_type()
            if not program_type:
                return
            
            # Opção de carregar arquivo de prompt
            prompt_refiner = PromptRefinerAgent(self.ai_client, self.config)
            prompt_file_path = prompt_refiner.get_prompt_from_file()
            
            # Obter descrição do usuário
            if prompt_file_path:
                print_colored("✅ Arquivo carregado. Você pode adicionar informações complementares:", "green")
                description = input("Informações adicionais (opcional): ").strip()
            else:
                description = self._get_description(
                    f"Descreva detalhadamente o programa {program_type} que você deseja criar"
                )
            
            if not description and not prompt_file_path:
                print_colored("❌ É necessário fornecer uma descrição ou arquivo de prompt.", "red")
                return
            
            # Executar pipeline de agentes
            results = self.orchestrator.execute_pipeline(
                user_prompt=description,
                generation_type="PROGRAM",
                prompt_file_path=prompt_file_path
            )
            
            # Obter código final
            final_code = results["reviewer"]["reviewed_code"]
            quality_score = results["reviewer"]["quality_score"]
            
            # Exibir código gerado
            print_colored("\n" + "="*70, "green")
            print_colored("📄 CÓDIGO ABAP GERADO", "green", bold=True)
            print_colored("="*70, "green")
            print_colored("```abap", "yellow")
            print(final_code)
            print_colored("```", "yellow")
            
            # Informações adicionais
            print_colored(f"\n⭐ Score de Qualidade: {quality_score}/100", "cyan")
            
            # Salvar arquivo
            filename = f"program_{program_type.lower().replace('-', '_')}"
            self._save_to_file(final_code, filename)
            
            # Mostrar próximos passos
            next_steps = results["developer"].get("next_steps", [])
            if next_steps:
                print_colored("\n📋 PRÓXIMOS PASSOS RECOMENDADOS:", "magenta", bold=True)
                for i, step in enumerate(next_steps, 1):
                    print_colored(f"   {i}. {step}", "white")
            
        except Exception as e:
            print_colored(f"❌ Erro ao gerar programa: {str(e)}", "red")
    
    def _select_program_type(self) -> str:
        """Seleciona tipo de programa"""
        print_colored("\n📋 Selecione o tipo de programa:", "cyan")
        
        for key, (prog_type, description) in self.PROGRAM_TYPES.items():
            print_colored(f"  {key}. {prog_type} - {description}", "white")
        
        try:
            choice = click.prompt(
                click.style("\nEscolha o tipo (1-5)", 
                           fg=self.config.get("text_color", "cyan")),
                type=click.Choice(list(self.PROGRAM_TYPES.keys()))
            )
            return self.PROGRAM_TYPES[choice][0]
        except (click.Abort, KeyboardInterrupt):
            return None