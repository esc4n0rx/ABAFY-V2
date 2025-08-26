from colorama import Fore, Style, init
from typing import Optional

# Inicializar colorama
init(autoreset=True)

# Mapeamento de cores
COLORS = {
    "red": Fore.RED,
    "green": Fore.GREEN,
    "yellow": Fore.YELLOW,
    "blue": Fore.BLUE,
    "magenta": Fore.MAGENTA,
    "cyan": Fore.CYAN,
    "white": Fore.WHITE,
    "black": Fore.BLACK,
    "reset": Fore.RESET
}

def get_color(color_name: str) -> str:
    """Obtém código de cor pelo nome"""
    return COLORS.get(color_name.lower(), Fore.WHITE)

def print_colored(
    text: str, 
    color: str = "white", 
    bold: bool = False, 
    end: str = "\n"
):
    """Imprime texto colorido"""
    color_code = get_color(color)
    style = Style.BRIGHT if bold else ""
    print(f"{style}{color_code}{text}{Style.RESET_ALL}", end=end)

def colorize(text: str, color: str = "white", bold: bool = False) -> str:
    """Retorna texto colorizado"""
    color_code = get_color(color)
    style = Style.BRIGHT if bold else ""
    return f"{style}{color_code}{text}{Style.RESET_ALL}"