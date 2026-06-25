### Fixed

- `Map<str, V>` への動的キー挿入後の retain/release が ArcHeader (-16) で行われ、StringHeader (-24) と offset が食い違う bug を修正。`for k in keys: m[k] = v` / `k = keys[i]; m[k] = v` / `m[rec.strField] = v` の三経路すべてに影響し、Linux/default-emit (release build) では JIT optimiser が固定アドレスへの UB store を残すため、3 件目あたりの挿入で `private constant` global の weak_count に書き込んで SIGSEGV していた。macOS/rust-emit では同じ UB が malloc 配置や Mach-O の rodata 扱いで偶発的に許容されていただけで、`RY_NO_OPT=1` でも観測可能。`retainArcValue` 経路を str 認識ベースで分岐させ、`for` ループ束縛・暗黙束縛・record field の三経路すべてが StringHeader (-24) へ正しく合流する。(#2375)
