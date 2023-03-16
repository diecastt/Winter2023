import java.util.*;

public class A2_Q4_final {

	public static double swaps(int[] passengers) {
		return 0;
	}
	public static double mergeSort(int[] passengers, int left, int right){
		double swaps = 0;

		if (right > left){
			int mid = (right + left)/2;

			// swaps to do in left subarray
			swaps += mergeSort(passengers, left, mid);

			//swaps to do in right subarray
			swaps += mergeSort(passengers, mid + 1, right);

			//swaps to do in merged array (at merging step)
			swaps += merge(passengers, left, mid, right);
		}
		return swaps;
	}
	
	public static double merge(int[] passengers, int left, int mid, int right){
		// Left subarray
		int[] left_arr = Arrays.copyOfRange(passengers, left, mid + 1);

		// Right subarray
		int[] right_arr = Arrays.copyOfRange(passengers, mid + 1, right + 1);

		int i = 0;
		int j = 0;
		int k = left;
		double swaps = 0;


		while (i < left_arr.length && j < right_arr.length){
			if (left_arr[i] <= right_arr[j])
				passengers[k++] = left_arr[i++];
			else {
				passengers[k++] = right_arr[j++];
				swaps += (mid + 1) - (left + i);
			}
		}
		while (i < left_arr.length)
			passengers[k++] = left_arr[i++];
		while (j < right_arr.length)
			passengers[k++] = right_arr[j++];
		return swaps;
	}
}