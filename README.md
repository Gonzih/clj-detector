# Fork of clj-detector project
## Adds operating system information to the user-agent function

Clojure interface to the UADetector `User-Agent` analyser.

## Usage

Add `clj-detector` to your project.clj.
You'll also need to reference the `uadetector-resources` release you'd like to use. For example:

```clojure
:dependencies [[me.gonzih.clj-detector "0.0.3"]
               [net.sf.uadetector/uadetector-resources "2014.10"]]
```

You can then parse a string with the `user-agent` function:

```clojure
(ns example
  (:require [clj-detector.core :refer [user-agent]]))

; windows desktop
(user-agent "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; WOW64; SLCC1; .NET CLR 2.0.50727; Media Center PC 5.0; .NET CLR 3.5.21022; .NET CLR 3.5.30729; .NET CLR 3.0.30618; MDDC; .NET4.0C; InfoPath.2; BRI/2)")
#clj_detector.core.Agent{:name "IE", :producer "Microsoft Corporation.", :family "IE", :type :browser, :version "7.0", :device :pc, :os-family "WINDOWS", :os-name "Windows Vista", :os-version "6.0"}

; ipad
(user-agent "Mozilla/5.0 (iPad; CPU OS 7_0_4 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) Version/7.0 Mobile/11B554a Safari/9537.53")
#clj_detector.core.Agent{:name "Mobile Safari", :producer "Apple Inc.", :family "MOBILE_SAFARI", :type :mobile-browser, :version "7.0", :device :tablet, :os-family "IOS", :os-name "iOS 7", :os-version "7.0.4"}

; google bot
(user-agent "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
#clj_detector.core.Agent{:name "Googlebot/2.1", :producer "Google Inc.", :family "GOOGLEBOT", :type :robot, :version "2.1", :device :other, :os-family "UNKNOWN", :os-name "unknown", :os-version ""}
```

## License

Copyright © 2014 Paul Ingles

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
