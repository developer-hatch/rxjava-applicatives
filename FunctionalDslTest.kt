package test

import arrow.fx.rx2.SingleK
import arrow.fx.rx2.extensions.fix
import core.withMonad
import dsl.*
import instances.MonadSingleK
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.reactivex.rxjava3.core.Single
import java.util.concurrent.TimeUnit

class FunctionalDslTest : StringSpec({

    "should compose applicative and monadic calls with nullable values" {
        data class TestData(val a: Int, val b: String, val c: String?)

        fun curried(): (Int) -> (String) -> (String?) -> TestData =
            { a -> { b -> { c -> TestData(a, b, c) } } }

        fun fetchA(): Single<Int> = Single.just(42).delay(50, TimeUnit.MILLISECONDS)
        fun fetchB(): Single<String> = Single.just("OK").delay(50, TimeUnit.MILLISECONDS)
        fun fetchC(): Single<String?>? = null

        val result = withMonad(MonadSingleK) {
            arrow.fx.rx2.extensions.singlek.monad.monad().just(curried())
                .zipApWith(fetchA())
                .zipApWith(fetchB())
                .flatApWithNullable(fetchC())
        }

        var finalResult: TestData? = null

        val latch = java.util.concurrent.CountDownLatch(1)

        result.fix().value.subscribe {
            finalResult = it
            latch.countDown()
        }

        latch.await(500, TimeUnit.MILLISECONDS)

        finalResult shouldBe TestData(42, "OK", null)
    }
})