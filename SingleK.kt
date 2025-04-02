package com.damianlattenero.kinds

import arrow.Kind
import io.reactivex.rxjava3.core.Single

class ForSingleK private constructor()
typealias SingleKOf<A> = Kind<ForSingleK, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> SingleKOf<A>.fix(): SingleK<A> = this as SingleK<A>

data class SingleK<A>(val value: Single<A>) : SingleKOf<A>