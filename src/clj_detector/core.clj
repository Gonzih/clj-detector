(ns clj-detector.core
  (:import [net.sf.uadetector.service UADetectorServiceFactory]
           [net.sf.uadetector UserAgent UserAgentType UserAgentFamily VersionNumber DeviceCategory ReadableDeviceCategory$Category OperatingSystemFamily])
  (:require [clojure.string :as st]))

(defprotocol ToClojure
  (to-clojure [x]))

(defn- parser []
  (UADetectorServiceFactory/getResourceModuleParser))

(defn user-agent [s]
  (to-clojure (.parse (parser) s)))

(extend-protocol ToClojure
  DeviceCategory
  (to-clojure [device]
    (condp = (.getCategory device)
      (ReadableDeviceCategory$Category/GAME_CONSOLE) :console
      (ReadableDeviceCategory$Category/OTHER) :other
      (ReadableDeviceCategory$Category/PDA) :pda
      (ReadableDeviceCategory$Category/PERSONAL_COMPUTER) :pc
      (ReadableDeviceCategory$Category/SMART_TV) :tv
      (ReadableDeviceCategory$Category/SMARTPHONE) :phone
      (ReadableDeviceCategory$Category/TABLET) :tablet
      (ReadableDeviceCategory$Category/UNKNOWN) :unknown))
  VersionNumber
  (to-clojure [version]
    (st/join "." (remove st/blank? (.getGroups version))))
  UserAgent
  (to-clojure [agent]
    (let [os (.getOperatingSystem agent)]
      {:name (.getName agent)
       :producer (.getProducer agent)
       :family (to-clojure (.getFamily agent))
       :type (to-clojure (.getType agent))
       :version (to-clojure (.getVersionNumber agent))
       :device (to-clojure (.getDeviceCategory agent))
       :os-family (to-clojure (.getFamily os))
       :os-name (.getName os)
       :os-version (to-clojure (.getVersionNumber os))}))
  UserAgentType
  (to-clojure [type]
    (condp = type
      (UserAgentType/BROWSER) :browser
      (UserAgentType/EMAIL_CLIENT) :email
      (UserAgentType/FEED_READER) :feed-reader
      (UserAgentType/LIBRARY) :library
      (UserAgentType/MEDIAPLAYER) :media-player
      (UserAgentType/MOBILE_BROWSER) :mobile-browser
      (UserAgentType/OFFLINE_BROWSER) :offline-browser
      (UserAgentType/OTHER) :other
      (UserAgentType/ROBOT) :robot
      (UserAgentType/UNKNOWN) :unknown
      (UserAgentType/USERAGENT_ANONYMIZER) :anonymizer
      (UserAgentType/VALIDATOR) :validator
      (UserAgentType/WAP_BROWSER) :wap-browser))
  UserAgentFamily
  (to-clojure [family] (.toString family))
  OperatingSystemFamily
  (to-clojure [family] (.toString family)))
