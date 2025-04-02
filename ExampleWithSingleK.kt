package com.damianlattenero.examples

import arrow.core.extensions.*
import com.damianlattenero.dsl.*
import com.damianlattenero.instances.MonadSingleK
import com.damianlattenero.kinds.*
import io.reactivex.rxjava3.core.Single
import java.util.concurrent.TimeUnit

data class User(
    val id: Int,
    val name: String,
    val email: String?,
    val phone: String,
    val address: String?
)

fun curriedUser(): (Int) -> (String) -> (String?) -> (String) -> (String?) -> User =
    { id -> { name -> { email -> { phone -> { address -> User(id, name, email, phone, address) } } } } }

fun fetchId(): SingleK<Int> = SingleK(Single.just(1).doOnSubscribe { println("Fetching ID...") }.delay(200, TimeUnit.MILLISECONDS))
fun fetchName(): SingleK<String> = SingleK(Single.just("Alice").doOnSubscribe { println("Fetching Name...") }.delay(200, TimeUnit.MILLISECONDS))
fun fetchEmail(): SingleK<String?>? = SingleK(Single.just("alice@example.com").doOnSubscribe { println("Fetching Email...") }.delay(200, TimeUnit.MILLISECONDS))
fun fetchPhone(): SingleK<String> = SingleK(Single.just("123456789").doOnSubscribe { println("Fetching Phone...") }.delay(200, TimeUnit.MILLISECONDS))
fun fetchAddress(): SingleK<String?>? = null // Simulate nullable absence

fun main() {
    with(MonadSingleK) {
        val result =
            (::curriedUser).just()
                .zipApWith(fetchId())
                .zipApWith(fetchName())
                .flatApWithNullable(fetchEmail())
                .zipApWith(fetchPhone())
                .flatApWithNullable(fetchAddress())

        result.fix().value.subscribe { println("Final User: $it") }
    }
}