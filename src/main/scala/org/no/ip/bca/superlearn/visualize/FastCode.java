package org.no.ip.bca.superlearn.visualize;

public class FastCode {
    public static double[] minmax(final double[] a) {
        double min = a[0];
        double max = a[0];
        for (int i = 1; i < a.length; i++) {
            final double aa = a[i];
            if (aa < min) {
                min = aa;
            } else if (aa > max) {
                max = aa;
            }
        }
        return new double[] { min, max };
    }
    
    public static void sub(final double[] a, final double[] b) {
        for (int i = 0; i < a.length; i++) {
            b[i] -= a[i];
        }
    }
}
