module Printer where

import Printer.PPrinter
import SMTLib2.Base

import UU.PPrint

prettyprint :: SMod -> Doc
prettyprint = spretty