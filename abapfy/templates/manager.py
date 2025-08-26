import json
from pathlib import Path
from typing import Dict, List, Any, Optional

class TemplateManager:
    """Gerenciador de templates ABAP"""
    
    def __init__(self):
        self.templates_dir = Path(__file__).parent
        self.template_cache = {}
    
    def get_available_templates(self, generation_type: str) -> List[Dict[str, Any]]:
        """Obtém lista de templates disponíveis por tipo"""
        templates = []
        
        if generation_type.upper() == "PROGRAM":
            programs_dir = self.templates_dir / "programs"
            if programs_dir.exists():
                for template_file in programs_dir.glob("*.abap"):
                    templates.append({
                        "name": template_file.name,
                        "path": str(template_file.relative_to(self.templates_dir)),
                        "type": "PROGRAM",
                        "description": self._extract_description(template_file)
                    })
        
        elif generation_type.upper() == "MODULE":
            modules_dir = self.templates_dir / "modules" 
            classes_dir = self.templates_dir / "classes"
            patterns_dir = self.templates_dir / "patterns"
            
            for directory in [modules_dir, classes_dir, patterns_dir]:
                if directory.exists():
                    for template_file in directory.glob("*.abap"):
                        templates.append({
                            "name": template_file.name,
                            "path": str(template_file.relative_to(self.templates_dir)),
                            "type": "MODULE",
                            "category": directory.name,
                            "description": self._extract_description(template_file)
                        })
        
        return templates
    
    def get_template_catalog(self) -> Dict[str, Any]:
        """Retorna catálogo completo com metadados dos templates"""
        catalog = {
            "programs": {},
            "modules": {},
            "classes": {},
            "patterns": {}
        }
        
        # Processar cada categoria
        for category in catalog.keys():
            category_dir = self.templates_dir / category
            if category_dir.exists():
                for template_file in category_dir.glob("*.abap"):
                    template_info = self._analyze_template(template_file)
                    catalog[category][template_file.name] = template_info
        
        return catalog
    
    def get_template_content(self, template_path: str) -> str:
        """Obtém conteúdo de um template específico"""
        full_path = self.templates_dir / template_path
        
        if full_path.exists():
            try:
                with open(full_path, 'r', encoding='utf-8') as f:
                    return f.read()
            except Exception:
                return ""
        
        return ""
    
    def _extract_description(self, template_file: Path) -> str:
        """Extrai descrição do template do cabeçalho"""
        try:
            with open(template_file, 'r', encoding='utf-8') as f:
                content = f.read()
                
                # Procurar por padrões de descrição
                lines = content.split('\n')[:10]  # Primeiras 10 linhas
                for line in lines:
                    if 'Descrição:' in line or 'Description:' in line:
                        return line.split(':', 1)[1].strip()
                    elif line.strip().startswith('*& ') and 'Report' not in line:
                        return line.replace('*&', '').strip()
                
                # Fallback: usar nome do arquivo
                return template_file.stem.replace('_', ' ').title()
                
        except Exception:
            return template_file.stem.replace('_', ' ').title()
    
    def _analyze_template(self, template_file: Path) -> Dict[str, Any]:
        """Analisa template e extrai metadados"""
        try:
            content = self.get_template_content(str(template_file.relative_to(self.templates_dir)))
            
            # Contar placeholders
            placeholders = content.count('{*')
            
            # Detectar padrões/recursos
            features = []
            if 'CL_SALV_TABLE' in content.upper():
                features.append('ALV')
            if 'SELECTION-SCREEN' in content.upper():
                features.append('Selection Screen')
            if 'AUTHORITY-CHECK' in content.upper():
                features.append('Authorization')
            if 'CLASS' in content.upper() and 'DEFINITION' in content.upper():
                features.append('Local Classes')
            if 'TRY.' in content.upper():
                features.append('Exception Handling')
            if 'BAPI' in content.upper():
                features.append('BAPI Integration')
            
            # Detectar tabelas referenciadas
            import re
            tables = re.findall(r'FROM\s+(\w+)', content.upper())
            tables = list(set(tables))[:5]  # Top 5 únicas
            
            return {
                "description": self._extract_description(template_file),
                "placeholders_count": placeholders,
                "features": features,
                "referenced_tables": tables,
                "complexity": "LOW" if placeholders < 5 else "MEDIUM" if placeholders < 15 else "HIGH",
                "lines_count": len(content.split('\n'))
            }
            
        except Exception:
            return {
                "description": "Template não analisável",
                "placeholders_count": 0,
                "features": [],
                "referenced_tables": [],
                "complexity": "UNKNOWN",
                "lines_count": 0
            }