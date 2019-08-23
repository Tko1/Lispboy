(defproject gameboy-emulator "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [gloss "0.2.6"]
                 ;; This project is a bit old, this will most likely be replaced with
                 ;; quill
                 [penumbra "0.6.0"]
                 [quil "3.0.0"]
                 [hask-tools "0.1.0-SNAPSHOT"]]
  :main gameboy-emulator.core)
