package com.wavesplatform.database

import java.util

import org.iq80.leveldb.{DB, DBIterator, ReadOptions}

import scala.annotation.tailrec

class ReadOnlyDB(db: DB, readOptions: ReadOptions) {
  def get[V](key: Key[V]): V       = key.parse(db.get(key.keyBytes, readOptions))
  def has[V](key: Key[V]): Boolean = db.get(key.keyBytes, readOptions) != null
  def iterator: DBIterator         = db.iterator(readOptions)

  def read[T](prefix: Array[Byte], seek: Array[Byte], n: Int)(deserialize: ReadOnlyDB.Entry => T): Vector[T] = {
    val iter = iterator
    @tailrec def loop(aux: Vector[T], restN: Int): Vector[T] = {
      if (restN > 0 && iter.hasNext) {
        val elem = iter.next()
        if (elem.getKey.startsWith(prefix)) loop(aux :+ deserialize(elem), restN - 1)
        else aux
      } else aux
    }

    try {
      iter.seek(seek)
      loop(Vector.empty, n)
    } finally iter.close()
  }
}

object ReadOnlyDB {
  type Entry = util.Map.Entry[Array[Byte], Array[Byte]]
}
