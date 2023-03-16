import java.util.*;

public class A2_Q2 {

	public static int change(int[] denominations) {
		//getting sum of largest two denominations
		int limit = denominations[denominations.length - 1] + denominations[denominations.length - 2];
		int[] dp = new int[limit + 1];
		int[] greedy = new int[limit + 1];

		for (int i = 0; i < limit + 1; i++){
			dp[i] = limit + 1;
			greedy[i] = limit + 1;
		}
		dp[0] = 0; //initial condition Do = 0
		greedy[0] = 1;

		for (int i = 1; i < limit + 1; i++){
			greedy[i] = greedy(denominations, i);
			for (int coin : denominations){
				if (i - coin >= 0) {
					dp[i] = Math.min(dp[i], 1 + dp[i - coin]);
				}
			}
			if (dp[i] != greedy[i]){
				return i;
			}
		}
		return -1;
	}

	public static int greedy(int[] denominations, int amount){
		int coins = 0;
		for (int i = denominations.length - 1; i >= 0; i--){
			coins += Math.floorDiv(amount, denominations[i]);
			amount = amount % denominations[i];
		}
		return coins;
	}

}
