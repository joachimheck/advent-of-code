(defproject advent-07 "0.1.0-SNAPSHOT"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [intcode "0.1.0-SNAPSHOT"]]
  :repl-options {:init-ns advent-07.core}
  :jvm-opts ["-Xmx2G" "-XX:-OmitStackTraceInFastThrow"])
