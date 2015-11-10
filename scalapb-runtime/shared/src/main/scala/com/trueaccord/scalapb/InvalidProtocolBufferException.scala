package com.trueaccord.scalapb

import java.io.IOException
import InvalidProtocolBufferException._

object InvalidProtocolBufferException {

  def truncatedMessage(): InvalidProtocolBufferException = {
    new InvalidProtocolBufferException("While parsing a protocol message, the input ended unexpectedly " +
      "in the middle of a field.  This could mean either that the " +
      "input has been truncated or that an embedded message " +
      "misreported its own length.")
  }

  def negativeSize(): InvalidProtocolBufferException = {
    new InvalidProtocolBufferException("CodedInputStream encountered an embedded string or message " +
      "which claimed to have negative size.")
  }

  def malformedVarint(): InvalidProtocolBufferException = {
    new InvalidProtocolBufferException("CodedInputStream encountered a malformed varint.")
  }

  def invalidTag(): InvalidProtocolBufferException = {
    new InvalidProtocolBufferException("Protocol message contained an invalid tag (zero).")
  }

  def invalidEndTag(): InvalidProtocolBufferException = {
    new InvalidProtocolBufferException("Protocol message end-group tag did not match expected tag.")
  }

  def invalidWireType(): InvalidProtocolBufferException = {
    new InvalidProtocolBufferException("Protocol message tag had invalid wire type.")
  }

  def recursionLimitExceeded(): InvalidProtocolBufferException = {
    new InvalidProtocolBufferException("Protocol message had too many levels of nesting.  May be malicious.  " +
      "Use CodedInputStream.setRecursionLimit() to increase the depth limit.")
  }

  def sizeLimitExceeded(): InvalidProtocolBufferException = {
    new InvalidProtocolBufferException("Protocol message was too large.  May be malicious.  " +
      "Use CodedInputStream.setSizeLimit() to increase the size limit.")
  }

  def parseFailure(): InvalidProtocolBufferException = {
    new InvalidProtocolBufferException("Failed to parse the message.")
  }

  def invalidUtf8(): InvalidProtocolBufferException = {
    new InvalidProtocolBufferException("Protocol message had invalid UTF-8.")
  }
}

@SerialVersionUID(-1616151763072450476L)
class InvalidProtocolBufferException(description: String) extends IOException(description)
