# abapfy/embeddings/chunker.py
import re
from typing import List, Dict
from dataclasses import dataclass

@dataclass
class CodeChunk:
    """Representa um chunk de código com metadata"""
    content: str
    start_line: int
    end_line: int
    chunk_type: str  # 'class', 'method', 'function', 'form', 'general'
    context: str = ""

class ABAPCodeChunker:
    """Divisor inteligente de código ABAP em chunks semânticos"""
    
    def __init__(self, max_chunk_size: int = 1000):
        self.max_chunk_size = max_chunk_size
        
        # Padrões para identificar blocos ABAP
        self.patterns = {
            'class_definition': re.compile(r'^\s*CLASS\s+(\w+)\s+DEFINITION', re.IGNORECASE | re.MULTILINE),
            'class_implementation': re.compile(r'^\s*CLASS\s+(\w+)\s+IMPLEMENTATION', re.IGNORECASE | re.MULTILINE),
            'method': re.compile(r'^\s*METHOD\s+(\w+)', re.IGNORECASE | re.MULTILINE),
            'function': re.compile(r'^\s*FUNCTION\s+(\w+)', re.IGNORECASE | re.MULTILINE),
            'form': re.compile(r'^\s*FORM\s+(\w+)', re.IGNORECASE | re.MULTILINE),
            'report': re.compile(r'^\s*REPORT\s+(\w+)', re.IGNORECASE | re.MULTILINE)
        }
        
        self.end_patterns = {
            'class': re.compile(r'^\s*ENDCLASS', re.IGNORECASE | re.MULTILINE),
            'method': re.compile(r'^\s*ENDMETHOD', re.IGNORECASE | re.MULTILINE),
            'function': re.compile(r'^\s*ENDFUNCTION', re.IGNORECASE | re.MULTILINE),
            'form': re.compile(r'^\s*ENDFORM', re.IGNORECASE | re.MULTILINE)
        }
    
    def chunk_code(self, code: str) -> List[CodeChunk]:
        """Divide código ABAP em chunks inteligentes"""
        lines = code.split('\n')
        chunks = []
        current_chunk_lines = []
        current_start_line = 1
        current_type = 'general'
        current_context = ""
        
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            
            # Verificar início de bloco especial
            block_info = self._identify_block_start(line, i + 1)
            
            if block_info:
                # Salvar chunk anterior se existir
                if current_chunk_lines:
                    chunks.append(self._create_chunk(
                        current_chunk_lines, current_start_line, 
                        i, current_type, current_context
                    ))
                    current_chunk_lines = []
                
                # Encontrar fim do bloco
                block_end = self._find_block_end(
                    lines, i, block_info['type'], block_info['name']
                )
                
                if block_end > i:
                    # Criar chunk para o bloco completo
                    block_lines = lines[i:block_end + 1]
                    chunks.append(CodeChunk(
                        content='\n'.join(block_lines),
                        start_line=i + 1,
                        end_line=block_end + 1,
                        chunk_type=block_info['type'],
                        context=f"{block_info['type'].title()}: {block_info['name']}"
                    ))
                    i = block_end + 1
                    current_start_line = i + 1
                    continue
            
            current_chunk_lines.append(lines[i])
            
            # Verificar se chunk atingiu tamanho máximo
            if len('\n'.join(current_chunk_lines)) > self.max_chunk_size:
                chunks.append(self._create_chunk(
                    current_chunk_lines, current_start_line, 
                    i + 1, current_type, current_context
                ))
                current_chunk_lines = []
                current_start_line = i + 2
            
            i += 1
        
        # Adicionar último chunk se existir
        if current_chunk_lines:
            chunks.append(self._create_chunk(
                current_chunk_lines, current_start_line, 
                len(lines), current_type, current_context
            ))
        
        return chunks
    
    def _identify_block_start(self, line: str, line_number: int) -> Dict:
        """Identifica início de bloco ABAP"""
        for pattern_name, pattern in self.patterns.items():
            match = pattern.match(line)
            if match:
                return {
                    'type': pattern_name.split('_')[0],  # 'class', 'method', etc.
                    'name': match.group(1) if match.lastindex else 'unknown',
                    'line': line_number
                }
        return None
    
    def _find_block_end(self, lines: List[str], start_idx: int, 
                       block_type: str, block_name: str) -> int:
        """Encontra fim do bloco ABAP"""
        end_pattern = self.end_patterns.get(block_type)
        if not end_pattern:
            return start_idx
        
        for i in range(start_idx + 1, len(lines)):
            if end_pattern.match(lines[i]):
                return i
        
        return len(lines) - 1
    
    def _create_chunk(self, lines: List[str], start_line: int, 
                     end_line: int, chunk_type: str, context: str) -> CodeChunk:
        """Cria objeto CodeChunk"""
        return CodeChunk(
            content='\n'.join(lines),
            start_line=start_line,
            end_line=end_line,
            chunk_type=chunk_type,
            context=context
        )
    
    def get_chunk_summary(self, chunks: List[CodeChunk]) -> str:
        """Gera resumo dos chunks para contexto da IA"""
        summary = []
        summary.append(f"Código dividido em {len(chunks)} chunks:\n")
        
        for i, chunk in enumerate(chunks, 1):
            lines_count = chunk.end_line - chunk.start_line + 1
            summary.append(
                f"Chunk {i}: {chunk.chunk_type} ({lines_count} linhas) - {chunk.context}"
            )
        
        return '\n'.join(summary)