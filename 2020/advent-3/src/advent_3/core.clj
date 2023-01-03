(ns advent-3.core)

; width height list
(define small-map (list 11 11
  "..##......."
  "#...#...#.."
  ".#....#..#."
  "..#.#...#.#"
  ".#...##..#."
  "..#.##....."
  ".#.#.#....#"
  ".#........#"
  "#.##...#..."
  "#...##....#"
  ".#..#...#.#"))

(define big-map (list 31 323
".....#.##......#..##..........#"
"##.#.##..#............##....#.."
"......###...#..............#.##"
".....#..##.#..#......#.#.#..#.."
"..#.......###..#..........#.#.."
"..#..#.##.......##.....#....#.."
".##....##....##.###.....###..#."
"..##....#...##..#....#.#.#....."
".....##..###.##...............#"
"#.....#..#....#.##...####..#..."
"#......#.#....#..#.##....#..#.#"
"##.#...#.#............#......#."
".#####.......#..#.#....#......#"
"..#.#....#.#.##...#.##...##...."
".....#.#...#..####.##..#......."
"#....#...##.#.#.##.#..##.....#."
"##.##...#....#...#......#..##.."
"....##...#..#.#...#.#.#.....##."
"..#....##......##....#.#....#.."
"#..#....#....###..#.##....#.#.#"
"..#.#####..##....#....#.....##."
".#...##.......#...#....#.#...##"
"#.#.#.##.......#.....#.#.#....#"
".#.#.....#.......#.......##...."
".#......#....#....#.......##..."
"#......#.....#......#..#..#...."
"#.#...#...#....##....#.#...#..#"
"....#.....##...#...#..#.#......"
"..#......#..........#...#.#...."
"..#..#......####..##...###....."
".#.....#...##...#.##........###"
"#.#....#..#....#..#.....#.#..#."
"...##.##.#.#.##...#.....#......"
"##....#.#.#...####.#.#.#.#....."
".##.........#..#..###.........."
"..##.###.#..#..#....##.....#..."
"##........#..###....#.#..#..#.."
"....#.#.......##..#.#.#.#......"
"....##.....#.........##.......#"
"..#........##.#.........###..##"
"....#..................##..#..."
"#...#.#..###..#.....#..#..#...#"
"..#..#.##..#..#.......#.......#"
".....#..##..#....##...........#"
"..##...#........#...#.#.......#"
".........#.#..#.#..#.##.#.###.."
"....#...#..#..#......##....#.#."
"..#..#.#....#....#..#.####..##."
"##....#.....#......##.###.#..#."
"#..#..##..###......#.#.#.#...#."
".......#..##..##...#...#..#...."
"..#.###.#...#....#.##.#.....##."
".#.#.......##...##...##....#..."
"#...#.#.#...#.####..#..##......"
"###..#.##..#..........#...#...."
"##.#.........#..##......####..."
"..##.#..#....#.##.............."
"...#....#.......###............"
"...#.....##....#.#.#.#.......##"
"###.###...#...#...###.##...##.."
"#.#....#.##..#.....#.....##.#.."
"...#....#....#.........#....#.#"
"##.#....#........#..#..##.#...."
".#.#..#.......#...##.......#..."
".##...##........#....#.#..#...."
"....#..#.##.###.....#.#........"
".#.#...#.#..#.....#.........#.."
".......#.#.#..##....#.........#"
".##...#....#..#...#........#..#"
"....#....#..#.#..#.#.#....##.##"
"..##....#.....##..#.#...#...#.."
"#.##.........#.....#.......#.##"
"...#...##.#.#..........#......#"
"###...#.....#..#.......#####..#"
"#.####...##.#.#..#...#........."
".##.....#.....##..#...##.##...."
".........###...#......##....###"
".#....##...###.#..#...##..#.#.#"
".......#.......#.#...##.#......"
".....#.#........#..##.....##..."
"....#.#.........##.#...##..#.#."
"#..#..#.##..#.##.##.....##.###."
"..##.........###...#....#....#."
".###...#..#.##...........#....."
"#..##..........#..........#...."
".....#.#....#..##..#...#.#....#"
"..#.....#.#....#...##.##......."
"##.....##........#....#..##...."
".#..#.#.........#..#..#........"
".............##....#....#..#..."
"....##....#..#.#.##....###.##.#"
".###..#.....#..#..##..#..##..#."
"...#..###.......#.#....#..###.."
"#.#..#.....#...#......#........"
"#..#..............###.#......#."
"..#....##.#....#.##.#.#...#...."
".........##..#...#.#.......#..."
"........#...#.#....#.....##..#."
"...#.##..#..#..###..#..#......#"
".....####......#...#....#...#.#"
"...###.#.#......#....#.......#."
"#...##.#....#....##....##.###.."
".......##...##.....#.##.#..#..#"
".....#.#............##...#.####"
".##..#.#.#.#..#.#.#.....#.##..."
".#..####...#.#....#.....#..#..."
"....##..#.#...#..#....#.#......"
"...#......###..#..###..#.....#."
".#.#.#..##....#...##..#.....#.."
"###....#....#...##.....#...#..."
"#.##....#......#...###........."
".#..#.#...#..#....#....#....#.."
"...............##...####..#..#."
"#.#...........####..#...##....."
"##.#....#........#......#...##."
"......#...#...#....#....#.....#"
"#......#.............#....###.."
".#...#...##.....#...##.##..#..."
"..#.#......#.#........#........"
".......#..#.#...##..#.#.#......"
"..##...#.##........#....#.#...#"
".....#..#..#........#.#......##"
"....#.#...##............##....#"
".#.#....#.#.#...#...#.##.....#."
"#.#.##...#....#.#.#..#.##..#.#."
".........####..#...#...#......."
"#..#..####......#..##..#...#..."
".........##..................#."
".....##.#..##.#.#...#......##.."
"...#....#....#.#.....#...#..#.#"
"#...##.#...##...........#..#..."
"#..........#.#..#..#.##..#..#.#"
".#...#.##...#.#.#..#.......##.."
".........#...........#..#..#..."
".##...##....#.#......#........#"
"#.#...........#....#.......#..."
"##.#.#.......#...###......##..#"
"...###..#.##..##.#.#.......#..."
".#...#..##.#...#........#.....#"
"...#.......#..#..........#.#..."
"..#.#.#.#.....#.#.......#..#..#"
"#.##.....#..##...#..###.#....#."
".......#...........#...#....###"
".......#..#...#.............#.."
"#.....###.......#...#........#."
".#..#..#..#...........#........"
"....#.#...#.#.##.#.#....#.##..#"
".......#..##...##...#...#......"
"...#.....##.###...#.#...##....#"
"#..#....#...##......#....##...."
"#.#.......#....#.###.##..#..#.."
"..##...........#...#....#......"
".#........#.....#..#..#...#..##"
".....#.#.#..#.......#....#....."
"#..#.#......#......##....#....."
"##.....................##......"
".##........###..#.........#...#"
"........#.........#..#........."
".#.##....#.....#...#.........##"
"....##......#.........#........"
"...#.#..#...##.##.#.#..####...."
"..##...........##.#.#....#....."
".#.....#.#...#..#.......#....#."
"....#...#......##...#...##.#..#"
"....#..##....#..#.........##.#."
"..##...##.##....#....##.###...#"
"..#....##..##.#.#.#...#......#."
"##...#.........#...........#..."
".##....##.#.....#...#.......#.."
"..........##.###.##....###....#"
"..........#..##..#....#.#.##.##"
"........##.#...#.#.#.#...###.#."
".#......#.#.#...###.#.#.#......"
".........#......#......#...#..#"
"......#.....#.##....##.#####..#"
"..#..##...###.#..........#.#.#."
".#..#....###.#...#..#....#...##"
"...................#..........#"
"....###.....#...##......#.....#"
"#.....#..##.....#.#..........#."
"..#.......##.#....#..#.##.#...#"
"........##.#..###..#......##..."
"#...........##.#...###..#....#."
"....#...........#.....#.#...#.."
".##..#.#...#...#.##...#..#....."
"#........#.#.#.#.#.#..........."
"#..#.....#..#..#.##....#....#.#"
"..#............##....#.#.##...#"
".....###.#....#.#......#.###..."
"...#.....#.#.................#."
"..#...##..#.#...#...#...#.....#"
".##.#........#..#....##..#..##."
".#..........#...#.#..#..#.#...."
"#.......##.........#.##..#.####"
".#..............#.......##....."
"#......#.##..........#..#......"
"..##...#...#.#...#............#"
".##.##..##..##........##.....#."
".....#..#.....##..............."
".#..#...##...#...#.....#......."
"#......#...#.......#..##.###.##"
"###..##......##......###....#.."
"....#..........#...#.##.#.....#"
".........#....#..#..#.#..##...."
".....#.....#...........#......#"
".#.......#...#....##...#.##...#"
"..##.#..............#..#...#.#."
".#..####.#.........#....#....#."
"..###.#...#..#......#.......###"
".#.#..##...###...#...#.#...#.#."
"...#..##..###.#..#.....#.##...."
"#...###.#...##.....####.....#.."
".#.##...#..#.#..##.....#......."
"...#.##.....##.....#....#......"
".#...##.....#..###..#.........."
"..........#...#.....#....##.#.."
".......#...#...#...#........#.."
"#....##..#...#..##.#.#.....#..."
".#.#..............#..#....#...."
".####.#.#.###......#...#.#....#"
".#...#...##.#...............#.#"
"...#.......##...#...#....##...."
"#..........###.##..........##.#"
".......#...#....#.#..#.#....#.."
"....#.##.#...###..#..##.##....."
"..#.#.#......#.#.......###....."
"#..................#.##....#..."
"#.....#..#.#.#..#...#.........#"
"..#..#...#.#.##........#......."
"#..#.#..#..........###...#.#..."
".......#.##....#........##.#..."
".####.#.#...#.#...##.##.....###"
"........#.#...#.#..##...##....."
"....##.##......#.##.........#.."
".#..#...#.#...........#........"
".......#..........#....#...#..."
"..###.#.###..#..#.....#..##...."
".#..........#.......##...#....."
".#.....#...#........#...#.##..#"
".#..#.......#..#.......#.#.#..."
"....#..##.#...##...#.#....#...."
".....#.........#..#..#....#...."
"..#.#..##....#..#..##.#.#.....#"
"........#.#...###....#.#.#....."
".#.....#.......#..###.#........"
".......#...#.#...#...##........"
"##.............#.#.....#.#..#.."
".#....#.......#.#.......#..##.."
"#.....#........#..##..##......."
"...........#.........###......#"
"....#.##...#.#...#...#....#..##"
"......#..##......#......#.##.#."
"......##....####...###...#....."
"#....#..........#.#.##.....#..#"
"....#.#...........#.#.#.#.#...#"
"....####.....##...#..##..#...#."
"#....#.###..###.....#..###....."
"..##.........#......#...##.#..."
"..#.....#.#...#.##.#...#..###.#"
"..#.##..##........#.......#.###"
".....#..........#.....#....#..."
".......##..##..###.......#####."
"..###......#.#....###....##...#"
"#..##.....#..###...#.....##.##."
"#..#..##.##.###.####.##.#......"
".#.#......#.##......#..#......#"
"..###.....#.#......#.#.####...."
"#..............#..#.#...#.###.."
"...#..#.##..........##.#...#.##"
".#.#.#.........#....#.#..#....."
"..#.##..#...#..#...#......#...."
".......#...#.##.#.#..#...##..#."
"..........#.####...#........#.#"
"....#...#....##.#.........#.#.."
"##.#..#.......###....#..#..#.#."
"..##.....#..#.#.#.####......#.."
".#.....#..........#..#..#.#...."
"......#.#.......#.#...#..#..#.."
"...#...............#....#...#.."
"##.......#.........#.......#..."
"...#.......###...#.#...#......."
"#...###....#....##....#....#..."
"...#....##..#.#.............##."
".....#.#.#..#......#...#.#..#.."
".##....#..##..#####..##.....##."
"....##.#.#..#.....#.#...#......"
"...#.....##.#.#..##..#.#......."
".......#..#..#..........#......"
".......#...#..#.........#.##..."
"..#..#..#...##..#.#....#......#"
"..#....#...#.#......#........#."
".#...#..#...#.#..........#....."
"#..#...####..#......##.##.#.#.."
".#...#.#...#.#.....#..##.#....."
"..#.##.#......#.........##...#."
"###..............#............."
"...#...###....#..#............."
".##....#......#..#.....#..#..#."
".#..........#.....##...#..#...."
"....##..#.#......###.##......#."
".#..##.#.##.#...##.#......###.#"
"#..###.#...###..#........#.#..."
"#..#.#.#..#...###.##.##..#..#.."
"#.#..#....#.........##......#.."
"....###.....###....#..........."
"....#..##.##....##..#.....#...."
".#.....#....####...#..##.#..###"
".........##..#......#.#...##..."
".##.......#.....#.###.#..#.#..#"
".....#.#...###.....#......####."
"##.#...#......#.#.#..#.####...#"
".#.##.....#..#..#.............#"
".#..###..#..#......#..##......#"
".......#.#........##.....#.#..."
"#....#...#..###..#.#.....#.##.."
".##.....#.#....###..#.....##..."
"...##....#....#...#....#.#.#..."
"#####..#...........###....#...#"
".#.......##.##.....#....#......"
".#..#.#...#..#......#...#..#.#."
"....#.....##...#####..#...#...#"
"###.##...#.#............#....#."
".....#...#........##.........#."
))

(define (last l)
  (if (null? l) '()
  (list-ref l (- (length l) 1))))

; Gets the value at the given position (. or #)
(define (get-value x y map)
  (let* ((width (car map))
         (height (cadr map))
         (rows (cddr map))
         (mod-x (mod x width))
         (row (list-ref rows y)))
    (string-ref row mod-x)))

; Generates the coordinates the toboggan will travel through.
(define (slope-positions startx starty dx dy endy)
  (if (= 1 endy) '((0 0))
  (let* ((result (slope-positions startx starty dx dy (- endy 1)))
         (x (car (last result)))
         (y (cadr (last result))))
    (append result (list (list (+ x dx) (+ y dy)))))))

(define (count-trees dx dy a-map)
  (let* ((height (cadr a-map))
         (width (car a-map))
         (positions (slope-positions 0 0 dx dy (floor (/ height dy))))
         (path (map (lambda (p) (get-value (mod (car p) width) (cadr p) a-map)) positions))
         (path-str (list->string path))
         (count (length (filter (lambda (c) (equal? c #\#)) path))))
    (display (list count path-str))
    (newline)))

; (count-trees 1 1 big-map) => 100
; (count-trees 3 1 big-map) => 276
; (count-trees 5 1 big-map) => 85
; (count-trees 7 1 big-map) => 90
; (count-trees 1 2 big-map) => 37
; (* 100 276 85 90 37) => 7812180000
