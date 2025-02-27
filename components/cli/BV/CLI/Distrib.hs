{-# LANGUAGE OverloadedStrings #-}

module BV.CLI.Distrib
    ( driverAddr
    , driverNodeId
    ) where

import Control.Distributed.Process (NodeId (NodeId))
import Network.Transport (EndPointAddress (EndPointAddress))

driverAddr :: EndPointAddress
driverAddr = EndPointAddress "driver"

driverNodeId :: NodeId
driverNodeId = NodeId driverAddr
