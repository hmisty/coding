import React from 'react'
import ReactDOM from 'react-dom'
import { Main, Header, Button, IconPlus, Tag, Split, Box, Card, IdentityBadge } from '@aragon/ui'
import { ethers } from 'ethers'
import PerpetualAuction from './artifacts/PerpetualAuction.json'
import { fs } from 'fs'

class App extends React.Component {
	constructor(props) {
		super(props);
		this.state = { 
			index: 0,
			tokenType: 'N/A'
		}
	}

	async componentDidMount() {
		const IERC721ABI = './artifacts/IERC721.abi';
		const IERC1155ABI = './artifacts/IERC1155.abi';
		const TokenType = { 0: 'ERC-721', 1: 'ERC-1155' };
		const auctionContractAddress = '0xF78B33770fD60284e783aFc0B6CCc3a7f2Cdc694';

		const provider = new ethers.providers.Web3Provider(window.ethereum);

		const auction = new ethers.Contract(auctionContractAddress, PerpetualAuction.abi, provider);
		const item = await auction.items(this.state.index);
		const nft = new ethers.Contract(item.token, IERC721, provider);
		const nftName = nft.name();

		this.setState({ 
			tokenType: TokenType[item.tokenType],
			tokenName: nftName,
		});
	}

	render() {
		return (
			<Main>
				<Header
					primary={
						<>
							{ this.state.tokenName }
							<Tag mode="identifier">{ this.state.tokenType }</Tag>
						</>
					}
					secondary={
						<IdentityBadge customLabel="Connect to a wallet" />
					}
				/>
			  <Split
					primary={
						<Box>
							<Card>Card content</Card>
						</Box>
					}
					secondary={
						<Box heading="Action">
							<ul class="actions">
								<li><Button wide mode="positive">Bid</Button></li>
								<li><Button wide mode="normal">Next</Button></li>
							</ul>
						</Box>
					}
				/>
			</Main>
		);
	}
}

export default App;
