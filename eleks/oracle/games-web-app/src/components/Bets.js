import { Table } from 'react-bootstrap';
import '../styles/Bets.scss';

const Bets = ({ bets }) => (
  <Table striped bordered hover className='Bets'>
    <thead>
      <tr>
        <th>#</th>
        <th>User</th>
        <th>Amount</th>
      </tr>
    </thead>
    <tbody>
      {bets.map((bet, i) => (
        <tr key={i}>
          <td>{i + 1}</td>
          <td>User name</td>
          <td>{bet.amount.getLovelace} Lovelace</td>
        </tr>
      ))}
    </tbody>
  </Table>
);

export default Bets;
