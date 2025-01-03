(defproject advent "0.1.0-SNAPSHOT"
  :description "Advent of Code"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :repl-options {:init-ns advent-08.core}
  :jvm-opts ["-Xmx1G" "-XX:-OmitStackTraceInFastThrow"])
