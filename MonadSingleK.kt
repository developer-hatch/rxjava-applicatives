package com.damianlattenero.instances

import arrow.Kind
import arrow.typeclasses.Monad
import com.damianlattenero.kinds.*

object MonadSingleK : Monad<ForSingleK> {
    override fun <A> just(a: A): SingleKOf<A> = SingleK(Single.just(a))

    override fun <A, B> Kind<ForSingleK, A>.flatMap(f: (A) -> Kind<ForSingleK, B>): SingleKOf<B> =
        fix().value.flatMap { (f(it) as SingleK<B>).value }.let { SingleK(it) }

    override fun <A, B> Kind<ForSingleK, A>.map(f: (A) -> B): SingleKOf<B> =
        fix().value.map(f).let { SingleK(it) }
}