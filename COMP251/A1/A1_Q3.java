import java.util.*;

public class A1_Q3 {

	public static int elements(int[] sizes) {
		int n = sizes.length;
		int ans = 0;
		Map<Integer, Integer> subarray = new HashMap<>();

		int left = 0;

		for (int right = 0; right < n; right++){
			if (subarray.containsKey(sizes[right])){
				left = Math.max(subarray.get(sizes[right]), left);
			}
			ans = Math.max(ans, right - left + 1);
			subarray.put(sizes[right], right + 1);
		}
		return ans;
	}

}
