package com.trueaccord.scalapb

import java.io.ByteArrayOutputStream
import java.io.IOException
import java.io.InputStream
import java.nio.ByteBuffer
import java.util.ArrayList
import java.util.Arrays
import java.util.List
import CodedInputStream._

object CodedInputStream {

  def newInstance(input: InputStream): CodedInputStream = new CodedInputStream(input)

  def newInstance(buf: Array[Byte]): CodedInputStream = newInstance(buf, 0, buf.length)

  def newInstance(buf: Array[Byte], off: Int, len: Int): CodedInputStream = {
    val result = new CodedInputStream(buf, off, len)
    result.pushLimit(len)
    result
  }

  def readRawVarint32(input: InputStream): Int = {
    val firstByte = input.read()
    if (firstByte == -1) {
      throw InvalidProtocolBufferException.truncatedMessage()
    }
    readRawVarint32(firstByte, input)
  }

  def readRawVarint32(firstByte: Int, input: InputStream): Int = {
    if ((firstByte & 0x80) == 0) {
      return firstByte
    }
    var result = firstByte & 0x7f
    var offset = 7
    while (offset < 32) {
      val b = input.read()
      if (b == -1) {
        throw InvalidProtocolBufferException.truncatedMessage()
      }
      result |= (b & 0x7f) << offset
      if ((b & 0x80) == 0) {
        return result
      }
      offset += 7
    }
    while (offset < 64) {
      val b = input.read()
      if (b == -1) {
        throw InvalidProtocolBufferException.truncatedMessage()
      }
      if ((b & 0x80) == 0) {
        return result
      }
      offset += 7
    }
    throw InvalidProtocolBufferException.malformedVarint()
  }

  def decodeZigZag32(n: Int): Int = (n >>> 1) ^ -(n & 1)

  def decodeZigZag64(n: Long): Long = (n >>> 1) ^ -(n & 1)

  private val DEFAULT_RECURSION_LIMIT = 100

  private val DEFAULT_SIZE_LIMIT = 64 << 20

  private val BUFFER_SIZE = 4096

  private trait RefillCallback {

    def onRefill(): Unit
  }
}

class CodedInputStream(private val buffer: Array[Byte], private var bufferPos: Int, len: Int) {
  def this(is: InputStream) {
    this(???, ???, ???)
    ???
  }

  def pushLimit(limit: Int) {
    ???
  }
}
