{-# LANGUAGE ForeignFunctionInterface #-}

module FFIo(

) where

import Foreign.Ptr
import Foreign.C

foreign import ccall "lets_be_rational set_implied_volatility_maximum_iterations" c_set_impl_vol_max_it :: Double -> Double
foreign import ccall "lets_be_rational set_implied_volatility_output_type" c_set_impl_vol_out_type :: Double -> Double
foreign import ccall "lets_be_rational set_implied_volatility_householder_method_order" c_set_impl_vol_hh_meth_ord :: Double -> Double
foreign import ccall "lets_be_rational normalised_black_call" c_norm_black_call :: Double -> Double -> Double
foreign import ccall "lets_be_rational normalised_vega" c_norm_vega :: Double -> Double -> Double
foreign import ccall "lets_be_rational normalised_black" c_norm_black :: Double -> Double -> Double -> Double
foreign import ccall "lets_be_rational black" c_black :: Double -> Double -> Double -> Double -> Double -> Double
foreign import ccall "lets_be_rational normalised_implied_volatility_from_a_transformed_rational_guess_with_limited_iterations" c_norm_impl_vol_fr_trans_guess_ltd :: Double -> Double -> Double -> Int -> Double
foreign import ccall "lets_be_rational normalised_implied_volatility_from_a_transformed_rational_guess" c_norm_impl_vol_fr_trans_guess :: Double -> Double -> Double -> Double
foreign import ccall "lets_be_rational implied_volatility_from_a_transformed_rational_guess_with_limited_iterations" c_impl_vol_fr_trans_guess_ltd :: Double -> Double -> Double -> Double -> Double -> Int -> Double
foreign import ccall "lets_be_rational implied_volatility_from_a_transformed_rational_guess" c_impl_vol_fr_trans_guess :: Double -> Double -> Double -> Double -> Double -> Double

h_black :: Double -> Double -> Double -> Double -> Double -> Double
h_black = c_black
