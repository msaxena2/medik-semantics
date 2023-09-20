from pathlib import Path

base_dir      = Path(__file__).parents[0]

medik_src_dir = base_dir      / 'kproj'
medik_src     = medik_src_dir / 'medik.md'
kompiled_dir  = base_dir      / '.build'   / 'llvm-exec' / 'medik-llvm-kompiled'
