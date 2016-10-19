-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides an implementation of the 'Langauge' datatype for the
-- java language. This implementation includes a type 'Java', which is a type
-- without content to be used as a phantom type, a java tokenizer, and other
-- things necessary to analyzer java code.

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Thesis.CodeAnalysis.Language.Java ( -- * Basic types
                                           Java
                                         , java
                                           -- * Index construction
                                         , buildIndexForJava) where

import Thesis.CodeAnalysis.Semantic
import Thesis.CodeAnalysis.Language.Java.Internal
import Thesis.CodeAnalysis.Language.Java.Internal.Index
