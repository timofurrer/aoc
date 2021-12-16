from pathlib import Path

from rich import print

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


import itertools


def grouper(iterable, n, fillvalue=None):
    "Collect data into non-overlapping fixed-length chunks or blocks"
    # grouper('ABCDEFG', 3, 'x') --> ABC DEF Gxx
    args = [iter(iterable)] * n
    return itertools.zip_longest(*args, fillvalue=fillvalue)


raw_hex_packet_stream = puzzle_input_raw.splitlines()[0]
raw_binary_repr_packet_stream = "".join(bin(int(x, 16))[2:].zfill(4) for x in raw_hex_packet_stream)

PACKET_VERSION_LENGTH = 3
PACKET_TYPE_ID_LENGTH = 3
PACKET_LITERAL_GROUP_LENGTH = 5
PACKET_LENGTH_TYPE_ID_LENGTH = 1
PACKET_TOTAL_LENGTH_LENGTH = 15
PACKET_NR_SUB_PACKET_LENGTH_LENGTH = 11

parse = lambda s, p, l: (int(s[p:(np := p + l)], 2), np)


def parse_single_packet(stream):
    pos = 0
    packet_version, pos = parse(stream, pos, PACKET_VERSION_LENGTH)
    packet_type_id, pos = parse(stream, pos, PACKET_TYPE_ID_LENGTH)

    packet = None
    if packet_type_id == 4:
        packet_literal_value_parts = [
            "".join(pg) for pg in itertools.takewhile(lambda x: x[0] == "1", grouper(stream[pos:], PACKET_LITERAL_GROUP_LENGTH))
        ]
        pos += len(packet_literal_value_parts) * PACKET_LITERAL_GROUP_LENGTH
        packet_literal_value_parts.append(stream[pos:pos + PACKET_LITERAL_GROUP_LENGTH])
        packet_literal_value = int("".join(p[1:] for p in packet_literal_value_parts), 2)
        packet = (packet_version, packet_type_id, packet_literal_value, [])
        pos += PACKET_LITERAL_GROUP_LENGTH
    else:
        subpackets = []
        length_type_id, pos = parse(stream, pos, PACKET_LENGTH_TYPE_ID_LENGTH)
        if length_type_id == 0:
            total_length, pos = parse(stream, pos, PACKET_TOTAL_LENGTH_LENGTH)
            subpackets_pos = 0
            while subpackets_pos < total_length:
                subpacket_pos, subpacket = parse_single_packet(stream[pos + subpackets_pos:pos + subpackets_pos + total_length])
                subpackets_pos += subpacket_pos
                subpackets.append(subpacket)

            packet = (packet_version, packet_type_id, None, subpackets)
            pos += subpackets_pos
        else:
            numbers_of_sub_packets, pos = parse(stream, pos, PACKET_NR_SUB_PACKET_LENGTH_LENGTH)
            subpackets_pos = 0
            for _ in range(numbers_of_sub_packets):
                subpacket_pos, subpacket = parse_single_packet(stream[pos + subpackets_pos:])
                subpackets_pos += subpacket_pos
                subpackets.append(subpacket)

            packet = (packet_version, packet_type_id, None, subpackets)
            pos += subpackets_pos

    return pos, packet


pos = 0
packets = []
while pos < len(raw_binary_repr_packet_stream) and not all(b == "0" for b in raw_binary_repr_packet_stream[pos:]):
    pos, packet = parse_single_packet(raw_binary_repr_packet_stream[pos:])
    packets.append(packet)


def process_packet_version(packet):
    version_sum = packet[0] + sum(process_packet_version(p) for p in packet[3])
    return version_sum

version_sum = sum(process_packet_version(p) for p in packets)
print(version_sum)
