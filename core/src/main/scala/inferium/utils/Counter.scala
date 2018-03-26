package inferium.utils

class Counter(private var current: Long = 0) {
    // TODO: make atomic
    def next(): Long = {
        val result = current
        current += 1
        return result
    }
}
