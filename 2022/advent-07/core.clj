(ns advent-07.core)

(require '[clojure.pprint :as pp])
(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(def small-input "small-input.txt")
(def large-input "large-input.txt")

(defn read-lines [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (doall(line-seq rdr))))

;; Part 1
;; What is the sum of the total sizes of all directories with sizes of at most 100000?
(defn group-inputs [lines]
  (reduce (fn [groups line]
            (if (= (first line) \$)
              (conj groups (vector line))
              (assoc groups (- (count groups) 1) (conj (last groups) line))))
          [] lines))

(defn is-directory? [s]
  (str/starts-with? s "dir"))

(defn directory-name [s]
  (subs s 4))

(defn parse-file [s]
  (let [splits (str/split s #" ")]
    (list (second splits) (Long/parseLong (first splits)))))

(defn update-path [path command]
  (let [dir (subs command 5)]
    (if (= ".." dir)
      (subvec path 0 (- (count path) 1))
      (conj path dir))))

(defn process-interaction [{path :path tree :tree :as state} interaction]
  (let [command (first interaction)]
    (if (= "$ ls" command)
      (let [subdirs (map directory-name (filter is-directory? (rest interaction)))
            files (map parse-file (filter #(not (is-directory? %)) (rest interaction)))
            with-dirs (reduce (fn [tree subdir]
                           (assoc-in tree (conj path subdir) {}))
                         tree subdirs)
            new-tree (assoc-in with-dirs (conj path :files) files)]
        (assoc state :tree new-tree))
      (assoc state :path (update-path path command)))))

(defn map-filesystem [f]
  (let [lines (read-lines f)
        interactions (group-inputs lines)
        tree {"/" {}}]
    (:tree (reduce process-interaction {:path [] :tree tree} interactions))))

(defn size-tree [[tree sizes :as state] path]
  (let [files (get-in tree (conj path :files))
        keys (filter #(not (= :files %)) (keys (get-in tree path)))
        subdir-paths (map #(conj path %) keys)
        recursive-state (reduce size-tree state subdir-paths)
        recursive-sizes (get recursive-state 1)
        subdir-sizes (map (fn [path] (get recursive-sizes path)) subdir-paths)]
    (vector tree
            (assoc recursive-sizes
                   path
                   (+ (reduce + (map second files))
                      (reduce + subdir-sizes))))))

;; (apply + (filter #(<= % 100000) (vals (second (size-tree [(map-filesystem small-input) {}] ["/"])))))
;; 95437
;; (apply + (filter #(<= % 100000) (vals (second (size-tree [(map-filesystem large-input) {}] ["/"])))))
;; 1667443


;; Part 2
;; What's the size of the smallest directory that can be deleted to free up enough space?
;;
;; (let [disk-size 70000000
;;                       required-space 30000000
;;                       sizes (second (size-tree [(map-filesystem small-input) {}] ["/"]))
;;                       total-size (get sizes ["/"])
;;                       available (- disk-size total-size)
;;                       need-to-delete (- required-space available)
;;                       candidates (filter (fn [[path size]] (> size need-to-delete)) sizes)
;;                       smallest (apply min (map second candidates))]
;;                   smallest)
;; 24933642
;;
;; (let [disk-size 70000000
;;       required-space 30000000
;;       sizes (second (size-tree [(map-filesystem large-input) {}] ["/"]))
;;       total-size (get sizes ["/"])
;;       available (- disk-size total-size)
;;       need-to-delete (- required-space available)
;;       candidates (filter (fn [[path size]] (> size need-to-delete)) sizes)
;;       smallest (apply min (map second candidates))]
;;   smallest)
;; 8998590
