from typing import Dict, Any, Callable
import time
from abapfy.agents.prompt_refiner import PromptRefinerAgent
from abapfy.agents.template_selector import TemplateSelectorAgent
from abapfy.agents.code_developer import CodeDeveloperAgent
from abapfy.agents.code_reviewer import CodeReviewerAgent
from abapfy.ai.client import AIClient
from abapfy.config.manager import ConfigManager
from abapfy.ui.colors import print_colored
from abapfy.utils.loading import LoadingContext, ProgressSpinner

class AgentOrchestrator:
    """Orquestrador dos agentes para geração de código ABAP"""
    
    def __init__(self, ai_client: AIClient, config: ConfigManager):
        self.ai_client = ai_client
        self.config = config
        
        # Inicializar agentes
        self.agents = {
            "refiner": PromptRefinerAgent(ai_client, config),
            "selector": TemplateSelectorAgent(ai_client, config),
            "developer": CodeDeveloperAgent(ai_client, config),
            "reviewer": CodeReviewerAgent(ai_client, config)
        }
        
        # Pipeline de execução
        self.pipeline_steps = [
            {
                "id": "refiner",
                "name": "🤖 Agente Refinador",
                "description": "Analisando e estruturando requisitos",
                "agent": self.agents["refiner"]
            },
            {
                "id": "selector", 
                "name": "🎯 Agente Seletor",
                "description": "Identificando templates adequados",
                "agent": self.agents["selector"]
            },
            {
                "id": "developer",
                "name": "👨‍💻 Agente Desenvolvedor", 
                "description": "Gerando código ABAP",
                "agent": self.agents["developer"]
            },
            {
                "id": "reviewer",
                "name": "🔍 Agente Revisor",
                "description": "Revisando e otimizando código",
                "agent": self.agents["reviewer"]
            }
        ]
    
    def execute_pipeline(self, user_prompt: str, generation_type: str, 
                        prompt_file_path: str = None) -> Dict[str, Any]:
        """Executa pipeline completo de geração"""
        
        print_colored("\n" + "="*70, "cyan")
        print_colored("🚀 INICIANDO PIPELINE DE AGENTES ABAPFY", "cyan", bold=True)
        print_colored("="*70, "cyan")
        
        # Dados inicial
        pipeline_data = {
            "user_prompt": user_prompt,
            "generation_type": generation_type,
            "prompt_file_path": prompt_file_path
        }
        
        results = {}
        
        # Executar cada etapa do pipeline
        for i, step in enumerate(self.pipeline_steps, 1):
            step_id = step["id"]
            step_name = step["name"] 
            step_description = step["description"]
            agent = step["agent"]
            
            # Header da etapa
            print_colored(f"\n📋 ETAPA {i}/4: {step_name}", "yellow", bold=True)
            print_colored(f"📝 {step_description}", "white")
            print_colored("-" * 50, "yellow")
            
            try:
                with LoadingContext(step_description, "spinner") as loader:
                    # Simular tempo de processamento
                    time.sleep(1)
                    
                    # Executar agente
                    step_result = agent.execute(pipeline_data)
                    
                    # Armazenar resultado
                    results[step_id] = step_result
                    
                    # Passar dados para próxima etapa
                    pipeline_data.update(step_result)
                
                # Mostrar resultado da etapa
                self._display_step_result(step_id, step_result)
                
            except Exception as e:
                print_colored(f"❌ Erro na etapa {step_name}: {str(e)}", "red")
                raise
        
        # Resultado final
        self._display_final_result(results)
        
        return results
    
    def _display_step_result(self, step_id: str, result: Dict[str, Any]):
        """Exibe resultado de uma etapa específica"""
        
        if step_id == "refiner":
            complexity = result.get("complexity_score", 0)
            templates_suggested = len(result.get("suggested_templates", []))
            
            print_colored(f"✅ Prompt refinado com sucesso", "green")
            print_colored(f"📊 Complexidade: {complexity}/10", "white")
            print_colored(f"🎯 Templates sugeridos: {templates_suggested}", "white")
            
        elif step_id == "selector":
            selected = result.get("selected_template")
            fallback = result.get("fallback_strategy", "")
            
            if selected:
                print_colored(f"✅ Template selecionado: {selected['name']}", "green")
                print_colored(f"📈 Score de compatibilidade: {selected.get('match_score', 0)}", "white")
            else:
                print_colored(f"🔄 Estratégia: {fallback}", "yellow")
                
        elif step_id == "developer":
            notes_count = len(result.get("implementation_notes", []))
            deps_count = len(result.get("dependencies", []))
            
            print_colored(f"✅ Código gerado com sucesso", "green") 
            print_colored(f"📝 Notas de implementação: {notes_count}", "white")
            print_colored(f"🔗 Dependências identificadas: {deps_count}", "white")
            
        elif step_id == "reviewer":
            quality_score = result.get("quality_score", 0)
            corrections = len(result.get("corrections_made", []))
            
            print_colored(f"✅ Revisão concluída", "green")
            print_colored(f"⭐ Score de qualidade: {quality_score}/100", "white")
            print_colored(f"🔧 Correções aplicadas: {corrections}", "white")
    
    def _display_final_result(self, results: Dict[str, Any]):
        """Exibe resultado final consolidado"""
        
        print_colored("\n" + "="*70, "green")
        print_colored("🎉 PIPELINE CONCLUÍDO COM SUCESSO!", "green", bold=True)
        print_colored("="*70, "green")
        
        # Estatísticas gerais
        final_code = results["reviewer"]["reviewed_code"]
        quality_score = results["reviewer"]["quality_score"]
        complexity = results["refiner"]["complexity_score"]
        
        print_colored(f"📊 ESTATÍSTICAS FINAIS:", "cyan", bold=True)
        print_colored(f"  📏 Linhas de código: {len(final_code.splitlines())}", "white")
        print_colored(f"   ⭐ Score de qualidade: {quality_score}/100", "white")
        print_colored(f"   🧮 Complexidade: {complexity}/10", "white")
        
        # Template usado
        selected_template = results["selector"].get("selected_template")
        if selected_template:
            print_colored(f"   🎯 Template utilizado: {selected_template['name']}", "white")
        else:
            print_colored(f"   🆕 Desenvolvimento from scratch", "white")
        
        # Correções aplicadas
        corrections = results["reviewer"]["corrections_made"]
        if corrections:
            print_colored(f"\n🔧 CORREÇÕES APLICADAS:", "yellow", bold=True)
            for i, correction in enumerate(corrections[:3], 1):
                print_colored(f"   {i}. {correction.get('issue', 'Correção aplicada')}", "white")
            if len(corrections) > 3:
                print_colored(f"   ... e mais {len(corrections)-3} correções", "yellow")
        
        # Próximos passos
        next_steps = results["developer"]["next_steps"]
        if next_steps:
            print_colored(f"\n📋 PRÓXIMOS PASSOS:", "magenta", bold=True)
            for i, step in enumerate(next_steps[:3], 1):
                print_colored(f"   {i}. {step}", "white")