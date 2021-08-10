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
		const { searchTerm, list } = this.state;

		return (
			<div className="App">
				<header className="App-header">
					<form>
						<input type="text" value={searchTerm} onChange={this.onSearchChange} />
					</form>
					<p>
						Check out "hello" + "4" = { concat("hello", "3") }.
					</p>
					<p>
						{ `Here's an interpolated string of concat("hello", "3") = ${ concat("hello", "3") }. Amazing?` }
					</p>

					{ list.filter(this.isSearched(searchTerm))
							.map((item: string) => 
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
