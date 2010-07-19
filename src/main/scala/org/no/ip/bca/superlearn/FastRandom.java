package org.no.ip.bca.superlearn;

import java.security.SecureRandom;

/**
 * This class is a modification of the {@link java.util.Random} class and contains a lot
 * of copied methods. As such this class is licensed GPLv2+classpath to fulfill
 * the requirements of the source class.
 */
class FastRandom {
    public static final SecureRandom SECURE_RANDOM = new SecureRandom();
    private final static long multiplier = 0x5DEECE66DL;
    private final static long addend = 0xBL;
    private final static long mask = (1L << 48) - 1;
    private long seed;
    
    public FastRandom() {
        this(SECURE_RANDOM.nextLong());
    }
    
    FastRandom(final long seed) {
        this.seed = (seed ^ multiplier) & mask;
    }
    
    private int next(final int bits) {
        seed = (seed * multiplier + addend) & mask;
        return (int) (seed >>> (48 - bits));
    }
    
    public double nextDouble() {
        return (((long) (next(26)) << 27) + next(27)) / (double) (1L << 53);
    }
    
    public long nextLong() {
        // it's okay that the bottom word remains signed.
        return ((long)(next(32)) << 32) + next(32);
    }
    
    public long nextLong(final long n) {
        final long l = nextLong();
        return (l & Long.MAX_VALUE) % n;
    }
}
