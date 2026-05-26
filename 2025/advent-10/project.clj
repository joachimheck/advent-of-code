(defproject advent "0.1.0-SNAPSHOT"
  :description "Advent of Code"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [org.ojalgo/ojalgo "56.1.1"]
                 [net.mikera/core.matrix "0.63.0"]
                 ;; [uncomplicate/neanderthal "0.62.0"]
                 [io.github.tonsky/vectorz-clj "0.48.1"]]
  :repl-options {:init-ns advent-10.core}
  :jvm-opts ["-Xmx1G" "-XX:-OmitStackTraceInFastThrow"])
