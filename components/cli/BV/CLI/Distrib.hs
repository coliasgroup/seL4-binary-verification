{-# LANGUAGE OverloadedStrings #-}

module BV.CLI.Distrib
    ( driverAddr
    ) where

import Network.Transport (EndPointAddress (EndPointAddress))

driverAddr :: EndPointAddress
driverAddr = EndPointAddress "driver"
