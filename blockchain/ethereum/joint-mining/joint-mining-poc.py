#!/usr/bin/env python3

# PoC of joint-mining

class Blockchain:

    def __init__(self):
        self.block_height = 0
        self.begin_block = 0 # from which block height mining begins
        self.block_reward = 10 # at a constant rate
        self.miners = range(3) # 3 miners
        self.assets = range(2) # 2 assets
        self.rates = [100] * len(self.assets) # conversion rates of the assets, default 1:100

        self.asset_miner = [[0] * len(self.miners) for _ in self.assets] # amount of assets for each miner
        self.asset_amount = [0] * len(self.assets) # total amount of assets
        self.total_value = 0 # weighted total value in the mining pool, wrt. conversion rates

        self.reward_factor = 0 # factor (per value) for calculating rewards
        self.remove_factor = [0] * len(self.assets) # factors (per value) for calculating removes

        # to be rewarded
        self.miner_reward = [0] * len(self.miners)
        # should be removed => actual reward = miner_reward - miner_remove
        self.miner_remove = [0] * len(self.miners)

        # actual reward already claimed
        self.miner_claimed = [0] * len(self.miners)

    def get_claimable_value(self, miner):
        #TODO sanity check
        mined_blocks = self.block_height - self.begin_block
        claimable = 0
        for i in self.assets:
            a = (self.reward_factor - self.remove_factor[i]) * mined_blocks * self.asset_miner[i][miner] * self.rates[i]
            claimable += a

        claimable -= self.miner_claimed[miner]
        return claimable

    def add_asset(self, miner, asset, amount):
        #TODO sanity check

        if self.total_value == 0:
            # restart from current block height
            self.begin_block = self.block_height

        # update data
        self.asset_miner[asset][miner] += amount
        self.asset_amount[asset] += amount
        self.total_value += amount * self.rates[asset]

        # calculate rewards
        mined_blocks = self.block_height - self.begin_block
        self.reward_factor += 1/self.total_value * self.block_reward

        return

    def remove_asset(self, miner, asset, amount):
        #TODO sanity check

        if self.total_value == 0:
            # restart from current block height
            self.begin_block = self.block_height

        # update data
        self.asset_miner[asset][miner] -= amount
        self.asset_amount[asset] -= amount
        self.total_value -= amount * self.rates[asset]

        # calculate rewards
        mined_blocks = self.block_height - self.begin_block
        self.reward_factor += 1/self.total_value * self.block_reward

        return

    def update_rate(self, asset, new_rate):
        #TODO sanity check

        # update data
        #old_rate = self.rates[asset]
        #self.rates[asset] = new_rate
        #self.total_value -= self.asset_amount[asset] * old_rate
        #self.total_value += self.asset_amount[asset] * self.rates[asset]

        return

    def checkpoint(self):
        claimable = [0] * len(self.miners)

        for i in self.miners:
            claimable[i] = self.get_claimable_value(i)

        print("#%d-%d %d %f %s" % (self.begin_block, self.block_height, self.total_value, self.reward_factor, str(claimable)))

    def run(self, nblocks):
        # simulate moving forward for nblocks blocks
        for block in range(nblocks):
            self.block_height += 1

            if self.block_height == 10:
                self.add_asset(0, 0, 1)

            #if self.block_height % 10 == 0:
            self.checkpoint()


if __name__ == '__main__':
    chain = Blockchain()
    chain.run(100)
    print("Current block height: %d" % chain.block_height)
