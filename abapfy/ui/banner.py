import os
from abapfy.ui.colors import print_colored

def show_banner():
    """Exibe o banner do ABAPFY"""
    # Limpar tela
    os.system('cls' if os.name == 'nt' else 'clear')
    
    banner = """
    ╔══════════════════════════════════════════════════════════════╗
    ║                                                              ║
    ║       █████╗ ██████╗  █████╗ ██████╗ ███████╗██╗   ██╗       ║
    ║      ██╔══██╗██╔══██╗██╔══██╗██╔══██╗██╔════╝╚██╗ ██╔╝       ║
    ║      ███████║██████╔╝███████║██████╔╝█████╗   ╚████╔╝        ║
    ║      ██╔══██║██╔══██╗██╔══██║██╔═══╝ ██╔══╝    ╚██╔╝         ║
    ║      ██║  ██║██████╔╝██║  ██║██║     ██║        ██║          ║
    ║      ╚═╝  ╚═╝╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝        ╚═╝          ║
    ║                                                              ║
    ║              CLI Tool para Desenvolvimento ABAP              ║
    ║                      Powered by AI                           ║
    ║                                                              ║
    ╚══════════════════════════════════════════════════════════════╝
    """
    
    print_colored(banner, "cyan", bold=True)
    print_colored("🚀 Bem-vindo ao ABAPFY v1.0.0", "green", bold=True)
    print_colored("🤖 Sua ferramenta inteligente para desenvolvimento ABAP\n", "yellow")