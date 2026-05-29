(defproject advent "0.1.0-SNAPSHOT"
  :description "Advent of Code"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [org.ojalgo/ojalgo "56.1.1"]
                 [net.mikera/core.matrix "0.63.0"]

                 [uncomplicate/neanderthal "0.62.0"]
                 [org.bytedeco/mkl "2025.2-1.5.12"
                  :classifier "linux-x86_64-redist"]
                 ;; [org.bytedeco/cuda "12.9-9.10-1.5.13-20250913.041224-9"
                 ;;  :classifier "windows-x86_64-redist"]
                 ;; [org.bytedeco/cuda "12.9-9.10-1.5.13-20250913.041224-9"
                 ;;  :classifier "windows-x86_64-redist-cublas"]

                 [io.github.tonsky/vectorz-clj "0.48.1"]]

;;  :repositories [["maven-central-snapshots" "https://central.sonatype.com/repository/maven-snapshots"]]

  :repl-options {:init-ns advent-10.core}
  :jvm-opts ["-Xmx1G" "-XX:-OmitStackTraceInFastThrow"])
