(defproject deriva "0.1.0-SNAPSHOT"
  :description "Automatic Differentiation for Java and Clojure"
  :url "http://lambder.com"  
  :scm {:url "git@github.com:lambder/Deriva.git"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.macro "0.1.2"]
                 [org.clojure/math.combinatorics "0.0.4"]
                 ]
  :profiles {                                                                 
             :dev {
              :aot [com.lambder.deriva.java] 
              :source-paths ["src/main/clojure"] 
              :java-source-paths ["src/main/java"] 
              :test-paths ["src/test/clojure"]
              :dependencies [ [midje "1.6-beta1"]]}

             :java-tests { :java-source-paths ["src/test/java"] :junit ["src/test/java"] :dependencies [[junit/junit "4.11"]]}                                                                        
             
             :provided [:dev]
             }
  
  
  :plugins [[lein-pprint "1.1.1"] [lein-marginalia "0.7.1"] [lein-midje "3.0.0"] [lein-junit "1.1.2"]]
  
  
  :marginalia {:javascript ["http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_HTMLorMML", "mathjax-config.js"]})