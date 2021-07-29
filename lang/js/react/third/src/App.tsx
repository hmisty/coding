//@flow
import React, { ChangeEvent } from 'react';
import './App.css';

function concat(a:string, b:string):string {
	return a + b;
}

const list: string[] = ['apple', 'pear', 'banana'];

type Props = {};
type State = {};

class App extends React.Component<Props, State> {

	state = {
		list,
		searchTerm: '',
	};

	onClick = (id: string) => {
		const updatedList = this.state.list.filter(item => `id-${item}` !== id);
		this.setState({ list: updatedList });
	};

	onSearchChange = (e: ChangeEvent<HTMLInputElement>) => {
		this.setState({ searchTerm: e.target.value });
	};

	isSearched = (searchTerm: string) => {
		return (item: string) => !searchTerm ||
			item.toLowerCase().includes(searchTerm.toLowerCase());
	};

	render() {
		return (
			<div className="App">
				<header className="App-header">
					<form>
						<input type="text" onChange={this.onSearchChange} />
					</form>
					<div>
						{ this.state.list.filter(this.isSearched(this.state.searchTerm)) }
					</div>
					<p>
						Check out "hello" + "4" = { concat("hello", "3") }.
					</p>
					<p>
						{ `Here's an interpolated string of concat("hello", "3") = ${ concat("hello", "3") }. Amazing?` }
					</p>

					{ this.state.list.map((item: string) => 
							<div key={ `id-${item}` }>
								<p>{item}</p>
								<button onClick={() => this.onClick(`id-${item}`)} type="button">
									Dismiss
								</button>
							</div>
						)
					}
				</header>
			</div>
		);
	}

}

export default App;
