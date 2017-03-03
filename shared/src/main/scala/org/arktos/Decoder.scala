package org.arktos

/**
 * Created by jp on 12.02.16.
 */
abstract class Decoder {
  def decode(s: String, charset: String = "UTF-8"): String
}
